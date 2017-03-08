package backend

import model.write.{ RaffleCreated, WinnersSelected }

class TwitterProjection extends SyncProjection {

  override def receiveEventSync: ReceiveEventSync = {
    case RaffleCreated(id)  => startStream()
    case _: WinnersSelected => stopStream()
  }

  def startStream() = println("starting to listen to twitter")
  def stopStream()  = println("stop to listen to twitter")
}
