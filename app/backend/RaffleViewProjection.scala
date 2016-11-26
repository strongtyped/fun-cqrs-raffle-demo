package backend
import io.funcqrs.Projection
import io.funcqrs.HandleEvent
import model.read.RaffleView
import model.read.Participant
import model.write.RaffleProtocol._

import scala.concurrent.Future

class RaffleViewProjection(repo: RaffleViewRepo) extends Projection {

  def handleEvent: HandleEvent = {

    case e: RaffleCreated =>
      Future.successful(repo.save(RaffleView(id = e.raffleId)))

    case e: RaffleUpdateEvent =>
      Future.successful {
        repo.updateById(e.raffleId) { lot =>
          updateFunc(lot, e)
        }
      }
  }

  private def updateFunc(view: RaffleView, evt: RaffleUpdateEvent): RaffleView = {
    evt match {

      case e: ParticipantAdded =>
        view.copy(participants = view.participants :+ newParticipant(e))

      case e: WinnerSelected =>
        view.copy(winners = e.winners, runDate = Some(e.date))

      case e: ParticipantRemoved =>
        view.copy(participants = view.participants.filter(_.name != e.name))

      case _ => view
    }
  }

  private def newParticipant(evt: ParticipantAdded): Participant = Participant(evt.name)
}
