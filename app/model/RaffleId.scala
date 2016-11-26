package model

import java.util.UUID

import io.funcqrs.AggregateId
import play.api.libs.json.Json

case class RaffleId(value: String) extends AggregateId

object RaffleId {
  implicit val format = Json.format[RaffleId]
  def random() = RaffleId(UUID.randomUUID().toString)
}