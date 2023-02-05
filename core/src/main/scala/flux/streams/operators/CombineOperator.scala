package flux.streams.operators

import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}

case class CombineOperator[T1, T2](o1: Observable[T1], o2: Observable[T2]) extends AbstractObservable[(T1, T2)] {
  var last1 = Option.empty[T1]
  var last2 = Option.empty[T2]
  var s1    = Option.empty[Subscriber[T1]]
  var s2    = Option.empty[Subscriber[T2]]

  private def combinedOnNext() = for {
    v1 <- last1
    v2 <- last2
  } subscribers.foreach(_.onNext((v1, v2)))

  override def onStart(): Unit = {
    val subscriber1 = new Subscriber[T1] {
      override def onNext(t: T1): Unit = {
        last1 = Some(t)
        combinedOnNext()
      }

      override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
    }
    o1.subscribe(subscriber1)
    s1 = Some(subscriber1)
    val subscriber2 = new Subscriber[T2] {
      override def onNext(t: T2): Unit = {
        last2 = Some(t)
        combinedOnNext()
      }

      override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
    }
    o2.subscribe(subscriber2)
    s2 = Some(subscriber2)
  }

  override def onStop(): Unit = {
    s1.foreach(o1.unsubscribe)
    s1 = None
    s2.foreach(o2.unsubscribe)
    s2 = None
  }
}
