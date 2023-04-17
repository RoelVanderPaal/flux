package flux.streams.constructor

import flux.streams.{Observable, Subscriber, Subscription}

case class BasicSubscription[T](observable: Observable[T], subscriber: Subscriber[T]) extends Subscription {
  def unsubscribe(): Unit = observable.unsubscribe(subscriber)
}
