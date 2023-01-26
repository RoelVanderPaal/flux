package flux.streams.operators

import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}

case class MergeOperator[T1, T2](o1: Observable[T1], o2: Observable[T2]) extends AbstractObservable[T1 | T2] {
  private var parentSubscriber = Option.empty[Subscriber[T1 | T2]]

  override def onStart(): Unit = {
    val subscriber = new Subscriber[T1 | T2] {
      override def onNext(t: T1 | T2): Unit = subscribers.foreach(_.onNext(t))

      override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
    }
    o1.subscribe(subscriber)
    o2.subscribe(subscriber)
    parentSubscriber = Some(subscriber)
  }

  override def onStop(): Unit = {
    parentSubscriber.foreach(o1.unsubscribe)
    parentSubscriber.foreach(o2.unsubscribe)
    parentSubscriber = None
  }

  def parents: Iterable[Observable[_]] = Iterable(o1, o2)

}
