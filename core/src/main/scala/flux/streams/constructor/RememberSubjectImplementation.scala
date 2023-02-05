package flux.streams.constructor

import flux.streams.{Subject, Subscriber, Subscription}

case class RememberSubjectImplementation[T]() extends AbstractObservable[T] with Subject[T] {
  var last = Option.empty[T]

  override def onStart(): Unit = {}

  override def onNext(t: T): Unit = {
    last = Some(t)
    subscribers.foreach(_.onNext(t))
  }

  override def onCompleted: Unit = subscribers.foreach(_.onCompleted)

  override def subscribe[S >: T](subscriber: Subscriber[S]): Subscription = {
    last.foreach(subscriber.onNext)
    super.subscribe(subscriber)
  }

}
