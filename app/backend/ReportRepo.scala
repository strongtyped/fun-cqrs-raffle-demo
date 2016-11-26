package backend

import model.RaffleId
import model.read.Report

class ReportRepo extends InMemoryRepository {

  type Model      = Report
  type Identifier = RaffleId

  /** Extract id from Model */
  protected def $id(model: Model): Identifier = model.id
}
