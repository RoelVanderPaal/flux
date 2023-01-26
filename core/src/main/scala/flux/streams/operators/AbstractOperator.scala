package flux.streams.operators

import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}

abstract class AbstractOperator[T, U](o: Observable[T]) extends AbstractObservable[U] {
  private var parentSubscriber = Option.empty[Subscriber[T]]

  def handleOnNext(subscribers: Iterable[Subscriber[U]], t: T): Unit

  override def onStart(): Unit = {
    val subscriber = new Subscriber[T] {
      override def onNext(t: T): Unit = handleOnNext(subscribers, t)

      override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
    }
    parentSubscriber = Some(subscriber)
    o.subscribe(subscriber)
  }

  override def onStop(): Unit = {
    parentSubscriber.foreach(o.unsubscribe)
    parentSubscriber = None
  }

  def parents: Iterable[Observable[_]] = Iterable(o)

  def debugInternal: Iterable[String] =
    Iterable(super.debug) ++ parents
      .flatMap {
        case ao: AbstractOperator[_, _] => ao.debugInternal
        case o                          => List(o.debug)
      }
      .map(v => s"\t$v")

  override def debug: String = debugInternal.mkString(System.lineSeparator())

}
