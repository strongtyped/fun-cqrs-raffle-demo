package backend
import model.read.{ Participant, RaffleView }
import model.write._

class RaffleViewProjection(repo: RaffleViewRepo) extends SyncProjection {

  def receiveEventSync: ReceiveEventSync = {

    case e: RaffleCreated =>
      repo.save(RaffleView(id = e.raffleId))

    case e: RaffleUpdateEvent =>
      repo.updateById(e.raffleId) { lot =>
        updateFunc(lot, e)
      }
  }

  private def updateFunc(view: RaffleView, evt: RaffleUpdateEvent): RaffleView = {
    evt match {

      case e: ParticipantAdded =>
        view.copy(participants = view.participants :+ newParticipant(e))

      case e: WinnersSelected =>
        view.copy(winners = e.winners, runDate = Some(e.date))

      case e: ParticipantRemoved =>
        view.copy(participants = view.participants.filter(_.name != e.name))

      case _ => view
    }
  }

  private def newParticipant(evt: ParticipantAdded): Participant = Participant(evt.name)
}
