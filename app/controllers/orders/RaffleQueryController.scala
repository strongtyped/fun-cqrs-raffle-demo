package controllers.orders

import backend.RaffleViewRepo
import model.RaffleId
import play.api.libs.json.Json
import play.api.mvc.{ Action, Controller }

class RaffleQueryController(raffleDetailsRepo: RaffleViewRepo) extends Controller {

  def view(id: String) = Action {
    raffleDetailsRepo
      .find(RaffleId(id))
      .map { raffle =>
        Ok(Json.toJson(raffle))
      }
      .getOrElse { NotFound(s"No raffle found for id $id") }
  }

  def list = Action {
    val raffles = raffleDetailsRepo.fetchAll
    Ok(Json.toJson(raffles))
  }
}
