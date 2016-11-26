package model.read

import java.time.OffsetDateTime

import model.RaffleId
import play.api.libs.json.Json

case class RaffleView(participants: List[Participant] = List(),
                      winners: List[String]           = List(),
                      runDate: Option[OffsetDateTime] = None,
                      id: RaffleId) {

  override def toString: String = {
    val participantsString =
      participants.map(_.name).mkString(" | ")
    s"""
       |RaffleView
       |  winners: ${winners.mkString(", ")}
       |  participants: $participantsString
       |  runDate: ${runDate.map(_.toString).getOrElse("")}
       |  id: $id
     """.stripMargin
  }
}

case class Participant(name: String)

object Participant {
  implicit val format = Json.format[Participant]
}

object RaffleView {
  implicit val format = Json.format[RaffleView]
}
