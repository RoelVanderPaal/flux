package flux.streams.constructor

import flux.streams.{Observable, Subscriber, Subscription}

abstract class AbstractObservable[T] extends Observable[T]() {
  private var subscribers = List.empty[Subscriber[T]]

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

  protected def handleNext(t: T)  = subscribers.foreach(_.onNext(t))
  protected def handleCompleted() = subscribers.foreach(_.onCompleted)

  override def debug: String = s"${this.toString} ==> ${subscribers.length}"
}
