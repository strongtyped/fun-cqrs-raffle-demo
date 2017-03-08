package controllers.raffle

import backend.RaffleAkkaBackend
import model.RaffleId
import model.write._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

class RaffleCommandController(orderAkkaBackend: RaffleAkkaBackend) extends Controller {

  case class Start(name: String)
  object Start {
    implicit val format = Json.format[Start]
  }

  // POST
  def createRaffle = Action.async(parse.json[Start]) { req =>
    val raffleId = RaffleId(req.body.name)
    val cmd      = CreateRaffle

    sendCommand(raffleId, cmd) {
      Created("Raffle created").withHeaders("Location" -> s"/raffle/${raffleId.value}")
    }
  }

  case class Subscription(participantName: String)
  object Subscription {
    implicit val format = Json.format[Subscription]
  }
  // -- Add a participant
  // POST
  def addParticipant(raffleId: String) =
    Action.async(parse.json[Subscription]) { req =>
      sendCommand(RaffleId(raffleId), AddParticipant(req.body.participantName)) {
        Ok("Participant added to raffle")
      }
    }
  // --------------------------------------------------------------------------

  // -- Remove a participant
  // DELETE
  def removeParticipant(num: String, name: String) = Action.async {
    sendCommand(RaffleId(num), RemoveParticipant(name)) {
      Ok("Participant removed from raffle")
    }
  }
  // --------------------------------------------------------------------------

  case class RunIt(numOfPrizes: Int)
  object RunIt {
    implicit val format = Json.format[RunIt]
  }
  // -- Run
  // POST
  def run(raffleId: String) = Action.async(parse.json[RunIt]) { req =>
    sendCommand(RaffleId(raffleId), Run(req.body.numOfPrizes)) {
      Ok("Raffle was run.")
    }
  }
  // --------------------------------------------------------------------------

  private def sendCommand(number: RaffleId, cmd: RaffleCommand)(response: => Result): Future[Result] = {
    (orderAkkaBackend.orderRef(number) ? cmd)
      .map { _ =>
        response
      }
      .recoverWith {
        case NonFatal(ex) =>
          Future.successful {
            BadRequest(
              Json.obj(
                "error" -> ex.getMessage,
                "type" -> ex.getClass.getSimpleName
              )
            )
          }
      }
  }
}
