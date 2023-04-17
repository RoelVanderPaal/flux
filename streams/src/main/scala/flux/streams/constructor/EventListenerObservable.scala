package flux.streams.constructor

import org.scalajs.dom.{Element, Event, EventTarget}

case class EventListenerObservable[T <: Event](element: EventTarget, `type`: String) extends AbstractObservable[T] {
  // forced conversion, otherwise removeEventListener does not work
  val listener: scalajs.js.Function1[T, Unit] = (e: T) => handleNext(e)

  override def onStart(): Unit = element.addEventListener[T](`type`, listener)
  override def onStop(): Unit  = element.removeEventListener[T](`type`, listener)
}
