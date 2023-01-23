package flux.streams.constructor

import flux.streams.{Observable, Subscriber, Subscription}

abstract class AbstractObservable[T] extends Observable[T] {
  var subscribers = List.empty[Subscriber[T]]

  def onStart(): Unit
  def onStop(): Unit = {}

  override def subscribe[S >: T](subscriber: Subscriber[S]): Subscription = {
    val empty = subscribers.isEmpty
    subscribers ::= subscriber
    if (empty) { onStart() }
    BasicSubscription(this, subscriber)
  }

  override def unsubscribe[S >: T](subscriber: Subscriber[S]): Unit = {
    subscribers = subscribers.filterNot(_ == subscriber)
    if (subscribers.isEmpty) {
      onStop()
    }
  }
}
