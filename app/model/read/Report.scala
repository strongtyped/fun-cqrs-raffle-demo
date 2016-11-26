package model.read

import play.api.libs.json.Json

case class Report(participants: List[String], doubleBooking: Map[String, Int] = Map.empty) {
  def addParticipant(name: String) = {
    if (participants.contains(name)) {
      val counter = doubleBooking.getOrElse(name, 0)
      copy(doubleBooking = doubleBooking + (name -> counter))
    } else {
      copy(participants = name :: participants)
    }
  }
}

object Report {
  implicit val format = Json.format[Report]
}
