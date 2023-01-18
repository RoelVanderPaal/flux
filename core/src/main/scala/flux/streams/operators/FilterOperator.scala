package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class FilterOperator[T](o: Observable[T], f: T => Boolean) extends AbstractOperator[T, T](o) {
  override def handleOnNext(subscribers: Iterable[Subscriber[T]], t: T): Unit = if (f(t)) {
    subscribers.foreach(_.onNext(t))
  }
}
