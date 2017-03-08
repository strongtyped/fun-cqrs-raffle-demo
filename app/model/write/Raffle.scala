package model.write

import java.time.OffsetDateTime

import io.funcqrs._
import io.funcqrs.behavior._
import io.funcqrs.behavior.handlers._
import model.RaffleId

import scala.util.Random

sealed trait Raffle {
  type Id = RaffleId
}

case class EmptyRaffle(id: RaffleId) extends Raffle {

  /**
    * Action: reject Run command if has no participants
    * Only applicable when list of participants is empty
    */
  def canNotRunWithoutParticipants =
    Raffle.actions
      .rejectCommand {
        // can't run if there is no participants
        case Run(_) =>
          new IllegalArgumentException("Raffle has no participants")
      }

  /**
    * Action: add a participant
    * Applicable as long as we don't have a winner
    */
  def acceptParticipants =
    Raffle.actions
      .commandHandler {
        OneEvent {
          case AddParticipant(name) => ParticipantAdded(name, id)
        }
      }
      .eventHandler {
        case ParticipantAdded(name, _) =>
          NonEmptyRaffle(
            participants = List(name),
            id           = id
          )
      }
}

case class NonEmptyRaffle(participants: List[String], id: RaffleId) extends Raffle {

  /**
    * Action: reject double booking. Can't add the same participant twice
    * Only applicable after adding at least one participant
    */
  def rejectDoubleBooking = {

    Raffle.actions
      .rejectCommand {
        // can't add participant twice
        case cmd: AddParticipant if hasParticipant(cmd.name) =>
          new IllegalArgumentException(s"Participant ${cmd.name} already added!")
      }
  }

  def hasParticipant(name: String) = participants.contains(name)

  /**
    * Action: add a participant
    * Applicable as long as we don't have a winner
    */
  def acceptParticipants =
    Raffle.actions
      .commandHandler {
        OneEvent {
          case AddParticipant(name) if hasParticipant(name) => DoubleBookingRejected(name, id)
          case AddParticipant(name)                         => ParticipantAdded(name, id)
        }
      }
      .eventHandler {
        case ParticipantAdded(name, _) => copy(participants = name :: participants)
      }
      .eventHandler { case DoubleBookingRejected(_, _) => this }

  /**
    * Action: remove participants (single or all)
    * Only applicable if Raffle has participants
    */
  def removeParticipants() =
    Raffle.actions
    // removing participants (single or all) produce ParticipantRemoved events
      .commandHandler {
        OneEvent { case RemoveParticipant(name) => ParticipantRemoved(name, id) }
      }
      .commandHandler {
        //  will produce a List[ParticipantRemoved]
        ManyEvents {
          case RemoveAllParticipants =>
            this.participants.map { name =>
              ParticipantRemoved(name, id)
            }
        }
      }
      .eventHandler {
        case ParticipantRemoved(name, _) =>
          val newParticipants = participants.filter(_ != name)
          // NOTE: if last participant is removed, transition back to EmptyRaffle
          if (newParticipants.isEmpty)
            EmptyRaffle(id)
          else
            copy(participants = newParticipants)
      }

  /**
    * Action: run the Raffle
    * Only applicable if it has at least one participant
    */
  def runTheRaffle =
    // format: off
    Raffle.actions
      .commandHandler {
        OneEvent {
          case Run(numOfPrizes) =>
            val winners = selectWinners(numOfPrizes)
            WinnersSelected(winners, OffsetDateTime.now, id)
        }
      }
      .eventHandler {
        // transition to end state on winner selection
      case WinnersSelected(winners, _, _) => FinishedRaffle(winners, id)
      }
    // format: on

  private def selectWinners(numOfPrizes: Int): List[String] = {

    def pickOne(candidates: List[String], selection: List[String], prizes: Int): List[String] = {
      if (candidates.isEmpty || prizes == 0) selection
      else {
        val index              = Random.nextInt(candidates.size)
        val winner             = candidates(index)
        val candidatesLeftOver = candidates.filter(_ != winner)
        pickOne(candidatesLeftOver, winner :: selection, prizes - 1)
      }
    }

    pickOne(participants, List(), numOfPrizes)
  }
}

case class FinishedRaffle(winners: List[String], id: RaffleId) extends Raffle {

  /**
    * Action: reject all
    * Applicable when a winner is selected. No new commands should be accepts.
    */
  def rejectAllCommands =
    // format: off
    Raffle.actions
      .rejectCommand {
        // no command can be accepted after having selected a winner
        case anyCommand  =>
          new RaffleHasAlreadyAWinner(s"Raffle has already a winner and the winner is $winners")
      }
    // format: on
}

/** Defines the Raffle Protocol, all Commands it may receive and Events it may emit */
// Commands ============================================================
sealed trait RaffleCommand
// Creation Command
case object CreateRaffle extends RaffleCommand

// Update Commands
case class AddParticipant(name: String) extends RaffleCommand

case class RemoveParticipant(name: String) extends RaffleCommand

case object RemoveAllParticipants extends RaffleCommand

case class Run(numOfPrizes: Int) extends RaffleCommand

// Events ============================================================
sealed trait RaffleEvent {
  def raffleId: RaffleId
}

// Creation Event
case class RaffleCreated(raffleId: RaffleId) extends RaffleEvent
// Update Events
sealed trait RaffleUpdateEvent extends RaffleEvent
case class ParticipantAdded(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
case class ParticipantRemoved(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
case class DoubleBookingRejected(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
case class WinnersSelected(winners: List[String], date: OffsetDateTime, raffleId: RaffleId) extends RaffleUpdateEvent

object Raffle extends Types[Raffle] {

  type Id      = RaffleId
  type Command = RaffleCommand
  type Event   = RaffleEvent

  // a tag for Raffle, useful to query the event store later on
  val tag = Tags.aggregateTag("Raffle")

  // defines seed command and event handlers
  def factory(raffleId: RaffleId) =
    // format: off
    actions
      .commandHandler { 
        OneEvent { 
          case CreateRaffle => RaffleCreated(raffleId)
        }
      }
      .eventHandler { 
        case RaffleCreated(_) => EmptyRaffle(id = raffleId)
      }
    // format: on

  def behavior(raffleId: RaffleId) =
    Behavior.construct {
      factory(raffleId)
    } andThen {
      case raffle: EmptyRaffle =>
        raffle.canNotRunWithoutParticipants ++
          raffle.acceptParticipants

      case raffle: NonEmptyRaffle =>
//        raffle.rejectDoubleBooking ++
        raffle.acceptParticipants ++
          raffle.removeParticipants ++
          raffle.runTheRaffle

      case raffle: FinishedRaffle => raffle.rejectAllCommands
    }
}

class RaffleHasAlreadyAWinner(msg: String) extends RuntimeException(msg)
