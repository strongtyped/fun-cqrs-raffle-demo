package controllers.orders

import backend.RaffleAkkaBackend
import model.RaffleId
import model.write.RaffleProtocol._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

class RaffleCommandController(orderAkkaBackend: RaffleAkkaBackend) extends Controller {

  case class Start(name: String, numOfPrizes: Int)
  object Start {
    implicit val format = Json.format[Start]
  }

  // POST
  def createOrder = Action.async(parse.json[Start]) { req =>
    val orderNumber = RaffleId(req.body.name)
    val cmd         = CreateRaffle(req.body.numOfPrizes)

    sendCommand(orderNumber, cmd) {
      Created("Raffle created").withHeaders("Location" -> s"/raffle/${orderNumber.value}")
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

  // -- Remove a participan
  // DELETE
  def removeParticipant(num: String, name: String) = Action.async {
    sendCommand(RaffleId(num), RemoveParticipant(name)) {
      Ok("Participant removed from raffle")
    }
  }
  // --------------------------------------------------------------------------

  // -- Pay order
  // POST
  def run(raffleId: String) = Action.async { req =>
    sendCommand(RaffleId(raffleId), Run) {
      Ok("Raffle was run.")
    }
  }
  // --------------------------------------------------------------------------

  private def sendCommand(number: RaffleId, cmd: RaffleCommand)(response: => Result): Future[Result] = {
    (orderAkkaBackend.orderRef(number) ? cmd).map { _ =>
      response
    }.recoverWith {
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
