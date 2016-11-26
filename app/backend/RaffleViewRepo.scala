package backend

import model.RaffleId
import model.read.RaffleView

class RaffleViewRepo extends InMemoryRepository {

  type Identifier = RaffleId
  type Model      = RaffleView

  /** Extract id from Model */
  protected def $id(view: RaffleView): RaffleId = view.id
}
