package week2

/**
 * Created by mateus on 6/9/15.
 */
trait Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var currentTime: Int = 0

  def getCurrentTime(): Int = currentTime

  def afterDelay(delay: Int)(block: => Unit) = {
    val event = Event(currentTime + delay, () => block)
    agenda = insert(agenda, event)
  }

  def insert(ag: Agenda, event: Event): Agenda = {
    ag match {
      case first :: rest if first.time <= event.time =>
        first :: insert(rest, event)
      case _ =>
        event :: ag
    }
  }

  private def loop(): Unit = {
    agenda match {
      case first :: rest =>
        agenda = rest
        currentTime = first.time
        first.action()
        loop()
      case Nil =>
    }
  }
  def run() = {
    afterDelay(0) {
      println(s"*** simulation started, time = $currentTime")
    }
    loop()
  }
}
