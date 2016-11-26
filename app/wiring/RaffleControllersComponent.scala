package wiring

import com.softwaremill.macwire._
import controllers.raffle.{ RaffleCommandController, RaffleQueryController }

trait RaffleControllersComponent extends RaffleComponent {

  lazy val orderController      = wire[RaffleCommandController]
  lazy val orderQueryController = wire[RaffleQueryController]

}
