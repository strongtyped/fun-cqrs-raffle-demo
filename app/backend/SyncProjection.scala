package backend

import io.funcqrs.{ DomainEvent, HandleEvent, Projection }

import scala.concurrent.Future

trait SyncProjection extends Projection {

  type ReceiveEventSync = PartialFunction[Any, Unit]

  def receiveEventSync: ReceiveEventSync

  def receiveEvent: ReceiveEvent = {
    case e if receiveEventSync.isDefinedAt(e) => Future.successful(receiveEventSync(e))
  }

}
