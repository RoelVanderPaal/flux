package flux.streams.operators
import flux.streams.{Observable, Subscriber, Subscription}

case class RememberOperator[T](o: Observable[T]) extends AbstractOperator[T, T](o) {
  var last                                                                    = Option.empty[T]
  override def handleOnNext(t: T): Unit = {
    last = Some(t)
    handleNext(t)
  }

  override def subscribe[S >: T](subscriber: Subscriber[S]): Subscription = {
    last.foreach(subscriber.onNext)
    super.subscribe(subscriber)
  }
}
