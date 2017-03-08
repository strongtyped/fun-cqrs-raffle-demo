package backend

import akka.persistence.journal.{ Tagged, WriteEventAdapter }
import model.write.{ Raffle, RaffleEvent }

class TagWriterEventAdapter extends WriteEventAdapter {

  def manifest(event: Any): String = ""

  def toJournal(event: Any): Any = {
    event match {

      // Raffle events get tagged with Raffle tag!
      case evt: RaffleEvent => Tagged(evt, Set(Raffle.tag.value))

      // other events are not tagged
      case evt => evt
    }
  }
}
