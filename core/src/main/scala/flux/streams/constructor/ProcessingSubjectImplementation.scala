package flux.streams.constructor

import flux.streams.{Observable, Subscriber, Subscription}

case class ProcessingSubjectImplementation[S, O](subscriber: Subscriber[S], observable: Observable[O])
    extends Subscriber[S]
    with Observable[O] {
  override def onNext(t: S): Unit = subscriber.onNext(t)

  override def onCompleted: Unit = subscriber.onCompleted

  override def subscribe[S >: O](subscriber: Subscriber[S]): Subscription = observable.subscribe(subscriber)

  override def unsubscribe[S >: O](subscriber: Subscriber[S]): Unit = observable.unsubscribe(subscriber)

  override def debug: String = observable.debug
}
