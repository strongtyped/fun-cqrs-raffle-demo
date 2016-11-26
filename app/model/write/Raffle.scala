package model.write

import java.time.OffsetDateTime
import io.funcqrs._
import io.funcqrs.behavior._
import model.RaffleId

import scala.util.Random

sealed trait Raffle extends AggregateLike {
  type Id       = RaffleId
  type Protocol = RaffleProtocol.type
}

case class EmptyRaffle(id: RaffleId) extends Raffle {

  import RaffleProtocol._

  /**
    * Action: reject Run command if has no participants
    * Only applicable when list of participants is empty
    */
  def canNotRunWithoutParticipants =
    // format: off
    action[Raffle]
      .rejectCommand {
        // can't run if there is no participants
        case _: Run =>
          new IllegalArgumentException("Raffle has no participants")
      }
    // format: on

  /**
    * Action: add a participant
    * Applicable as long as we don't have a winner
    */
  def acceptParticipants =
    // format: off
    actions[Raffle]
      .handleCommand { cmd: AddParticipant =>
        ParticipantAdded(cmd.name, id)
      }
      .handleEvent { evt: ParticipantAdded =>
        NonEmptyRaffle(
          participants = List(evt.name),
          id           = id
        )
      }
    // format: on
}

case class NonEmptyRaffle(participants: List[String], id: RaffleId) extends Raffle {

  import RaffleProtocol._

  /**
    * Action: reject double booking. Can't add the same participant twice
    * Only applicable after adding at least one participant
    */
  def rejectDoubleBooking = {

    // format: off
    action[Raffle]
      .rejectCommand {
        // can't add participant twice
        case cmd: AddParticipant if hasParticipant(cmd.name) =>
          new IllegalArgumentException(s"Participant ${cmd.name} already added!")
      }
    // format: on
  }

  def hasParticipant(name: String) = participants.contains(name)

  /**
    * Action: add a participant
    * Applicable as long as we don't have a winner
    */
  def acceptParticipants =
    // format: off
    actions[Raffle]
      .handleCommand { cmd: AddParticipant =>
        if (hasParticipant(cmd.name))
          DoubleBookingRejected(cmd.name, id)
        else 
          ParticipantAdded(cmd.name, id)
      }
      .handleEvent { evt: ParticipantAdded =>
        copy(participants = evt.name :: participants)
      }
      .handleEvent { _: DoubleBookingRejected => this}
    // format: on

  /**
    * Action: remove participants (single or all)
    * Only applicable if Raffle has participants
    */
  def removeParticipants() =
    // format: off
    actions[Raffle]
      // removing participants (single or all) produce ParticipantRemoved events
      .handleCommand { cmd: RemoveParticipant =>
        ParticipantRemoved(cmd.name, id)
      }
      .handleCommand {
        // will produce a List[ParticipantRemoved]
        cmd: RemoveAllParticipants.type =>
          this.participants.map { name =>
            ParticipantRemoved(name, id)
          }
      }
      .handleEvent { evt: ParticipantRemoved =>
        val newParticipants = participants.filter(_ != evt.name)
        // NOTE: if last participant is removed, transition back to EmptyRaffle
        if (newParticipants.isEmpty)
          EmptyRaffle(id)
        else
          copy(participants = newParticipants)
      }
    // format: off

  /**
    * Action: run the Raffle
    * Only applicable if it has at least one participant
    */
  def runTheRaffle =
    // format: off
    actions[Raffle]
      .handleCommand {
        cmd: Run =>
          val winners = selectWinners(cmd.numOfPrizes)
          WinnerSelected(winners, OffsetDateTime.now, id)
      }
      .handleEvent {
        // transition to end state on winner selection
        evt: WinnerSelected => FinishedRaffle(evt.winners, id)
      }
    // format: on

  private def selectWinners(numOfPrizes: Int): List[String] = {

    def pickOne(candidates: List[String], selection: List[String]): List[String] = {
      if (candidates.isEmpty) selection
      else if (selection.size < numOfPrizes) {
        val index  = Random.nextInt(candidates.size)
        val winner = participants(index)
        val cand   = participants.filter(_ != winner)
        pickOne(cand, winner :: selection)
      } else selection
    }

    pickOne(participants, List())
  }
}

case class FinishedRaffle(winners: List[String], id: RaffleId) extends Raffle {

  /**
    * Action: reject all
    * Applicable when a winner is selected. No new commands should be accepts.
    */
  def rejectAllCommands =
    // format: off
    action[Raffle]
      .rejectCommand {
        // no command can be accepted after having selected a winner
        case anyCommand  =>
          new RaffleHasAlreadyAWinner(s"Raffle has already a winner and the winner is $winners")
      }
    // format: on
}

/** Defines the Raffle Protocol, all Commands it may receive and Events it may emit */
object RaffleProtocol extends ProtocolLike {

  // Commands ============================================================
  sealed trait RaffleCommand extends ProtocolCommand
  // Creation Command
  case object CreateRaffle extends RaffleCommand

  // Update Commands
  case class AddParticipant(name: String) extends RaffleCommand

  case class RemoveParticipant(name: String) extends RaffleCommand

  case object RemoveAllParticipants extends RaffleCommand

  case class Run(numOfPrizes: Int) extends RaffleCommand

  // Events ============================================================
  sealed trait RaffleEvent extends ProtocolEvent {
    def raffleId: RaffleId
  }

  // Creation Event
  case class RaffleCreated(raffleId: RaffleId) extends RaffleEvent
  // Update Events
  sealed trait RaffleUpdateEvent extends RaffleEvent
  case class ParticipantAdded(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
  case class ParticipantRemoved(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
  case class DoubleBookingRejected(name: String, raffleId: RaffleId) extends RaffleUpdateEvent
  case class WinnerSelected(winners: List[String], date: OffsetDateTime, raffleId: RaffleId) extends RaffleUpdateEvent

}

object Raffle {

  // import the protocol to have access to Commands and Events
  import RaffleProtocol._

  // a tag for Raffle, useful to query the event store later on
  val tag = Tags.aggregateTag("Raffle")

  // defines seed command and event handlers
  def factory(raffleId: RaffleId) =
    // format: off
    actions[Raffle]
      .handleCommand { cmd: CreateRaffle.type =>
        RaffleCreated(raffleId)
      }
      .handleEvent { evt: RaffleCreated =>
        EmptyRaffle(id = raffleId)
      }
    // format: on

  def behavior(raffleId: RaffleId): Behavior[Raffle] =
    Behavior {
      factory(raffleId)
    } {
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
