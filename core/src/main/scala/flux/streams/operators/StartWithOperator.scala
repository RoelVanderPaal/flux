package flux.streams.operators

import flux.streams.{Observable, Subscriber, Subscription}

case class StartWithOperator[T, U >: T](o: Observable[T], start: U) extends AbstractOperator[U, U](o) {
  var started                                                                 = false
  override def handleOnNext(subscribers: Iterable[Subscriber[U]], t: U): Unit = {
    subscribers.foreach(_.onNext(t))
  }

  override def subscribe[S >: U](subscriber: Subscriber[S]): Subscription = {
    if (!started && subscribers.isEmpty) {
      subscriber.onNext(start)
      started = true
    }
    super.subscribe(subscriber)
  }
}
