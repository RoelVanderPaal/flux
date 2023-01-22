package flux.streams.operators
import flux.streams.{Observable, Subscriber}

case class RememberOperator[T](o: Observable[T]) extends AbstractOperator[T, T](o) {
  var last                                                                    = Option.empty[T]
  override def handleOnNext(subscribers: Iterable[Subscriber[T]], t: T): Unit = {
    last = Some(t)
    subscribers.foreach(_.onNext(t))
  }

  override def subscribe[S >: T](subscriber: Subscriber[S]): Unit = {
    last.foreach(subscriber.onNext)
    super.subscribe(subscriber)
  }
}
