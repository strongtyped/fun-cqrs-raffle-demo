package wiring

import akka.actor.ActorSystem
import com.softwaremill.macwire._
import io.funcqrs.backend.QueryByTag
import io.funcqrs.config.Api._
import backend._
import model.write.Raffle

trait RaffleComponent {

  def actorSystem: ActorSystem

  lazy val raffleBackend = wire[RaffleAkkaBackend]

  lazy val raffleDetailsRepo = wire[RaffleViewRepo]
  lazy val reportRepo        = wire[ReportRepo]

  raffleBackend
    .configure {
      aggregate(Raffle.behavior)
    }
    .configure {
      projection(
        query      = QueryByTag(Raffle.tag),
        projection = wire[RaffleViewProjection]
      )
    }
    .configure {
      projection(
        query      = QueryByTag(Raffle.tag),
        projection = wire[ReportProjection]
      )
    }
}
