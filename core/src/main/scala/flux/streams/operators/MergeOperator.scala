package flux.streams.operators

import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}

case class MergeOperator[T](observables: Iterable[Observable[T]]) extends AbstractObservable[T] {
  private var parentSubscriber = Option.empty[Subscriber[T]]

  override def onStart(): Unit = {
    val subscriber = new Subscriber[T] {
      override def onNext(t: T): Unit = subscribers.foreach(_.onNext(t))

      override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
    }
    observables.foreach(_.subscribe(subscriber))
    parentSubscriber = Some(subscriber)
  }

  override def onStop(): Unit = {
    parentSubscriber.foreach(s => observables.foreach(_.unsubscribe(s)))
    parentSubscriber = None
  }

  def parents: Iterable[Observable[_]] = observables
}
