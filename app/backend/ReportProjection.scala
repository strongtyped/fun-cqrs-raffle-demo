package backend

import model.RaffleId
import model.read.Report
import model.write.RaffleProtocol.{ DoubleBookingRejected, ParticipantAdded, RaffleCreated }

class ReportProjection(repo: ReportRepo) extends SyncProjection {

  def handleEventSync: HandleEventSync = {
    case evt: RaffleCreated         => repo.save(Report(evt.raffleId))
    case evt: ParticipantAdded      => updateReport(evt.raffleId, evt.name)
    case evt: DoubleBookingRejected => updateReport(evt.raffleId, evt.name)
  }

  def updateReport(id: RaffleId, name: String) =
    repo
      .updateById(id) { report =>
        report.add(name)
      }
      .map(_ => Unit)
}
