package flux.streams.operators

import flux.streams.{Observable, Subscriber, Subscription}

case class StartWithOperator[T, U >: T](o: Observable[T], start: U) extends AbstractOperator[U, U](o) {
  var started                           = false
  override def handleOnNext(t: U): Unit = {
    handleNext(t)
  }

  override def subscribe[S >: U](subscriber: Subscriber[S]): Subscription = {
    if (!started) {
      subscriber.onNext(start)
      started = true
    }
    super.subscribe(subscriber)
  }
}
