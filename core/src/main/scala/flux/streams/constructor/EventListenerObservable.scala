package flux.streams.constructor

import org.scalajs.dom.{Element, Event, EventTarget}

case class EventListenerObservable[T <: Event](element: EventTarget, `type`: String) extends AbstractObservable[T] {
  val listener = (e: T) => subscribers.foreach(_.onNext(e))

  override def onStart(): Unit = element.addEventListener[T](`type`, listener)

  override def onStop(): Unit = element.removeEventListener[T](`type`, listener)
}
