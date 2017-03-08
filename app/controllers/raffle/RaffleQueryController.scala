package controllers.raffle

import backend.{ RaffleViewRepo, ReportRepo }
import model.RaffleId
import play.api.libs.json.Json
import play.api.mvc.{ Action, Controller }

class RaffleQueryController(raffleDetailsRepo: RaffleViewRepo, reportRepo: ReportRepo) extends Controller {

  def view(id: String) = Action {
    raffleDetailsRepo
      .find(RaffleId(id))
      .map { raffle =>
        Ok(Json.toJson(raffle))
      }
      .getOrElse { NotFound(s"No raffle found for id $id") }
  }

  def winners(id: String) = Action {
    raffleDetailsRepo
      .find(RaffleId(id))
      .map { raffle =>
        Ok(Json.toJson(raffle.winners))
      }
      .getOrElse { NotFound(s"No raffle found for id $id") }
  }

  def list = Action {
    val raffles = raffleDetailsRepo.fetchAll
    Ok(Json.toJson(raffles))
  }

  def report(id: String) = Action {
    reportRepo
      .find(RaffleId(id))
      .map { report =>
        Ok(Json.toJson(report))
      }
      .getOrElse { NotFound(s"No report found for id $id") }
  }
}
