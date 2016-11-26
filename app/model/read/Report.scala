package model.read

import model.RaffleId
import play.api.libs.json.Json

case class Report(id: RaffleId, participants: List[String] = List.empty, doubleBooking: Map[String, Int] = Map.empty) {

  def add(name: String) = {
    if (participants.contains(name)) {
      val counter = doubleBooking.getOrElse(name, 0) + 1
      copy(doubleBooking = doubleBooking + (name -> counter))
    } else {
      copy(participants = name :: participants)
    }
  }
}

object Report {
  implicit val format = Json.format[Report]
}
