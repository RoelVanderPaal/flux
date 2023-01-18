package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class DropOperator[T](o: Observable[T], n: Int) extends AbstractOperator[T, T](o) {
  private var dropped                                                         = 0
  override def handleOnNext(subscribers: Iterable[Subscriber[T]], t: T): Unit =
    if (dropped < n) { dropped += 1 }
    else { subscribers.foreach(_.onNext(t)) }
}
