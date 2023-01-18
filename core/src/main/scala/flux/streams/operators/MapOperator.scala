package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class MapOperator[T, U](o: Observable[T], f: T => U) extends AbstractOperator[T, U](o) {
  override def handleOnNext(subscribers: Iterable[Subscriber[U]], t: T): Unit = {
    val u = f(t)
    subscribers.foreach(_.onNext(u))
  }
}
