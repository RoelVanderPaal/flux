package flux.streams.constructor

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers.*

case class PeriodicObservable[T](interval: Interval, i: Iterator[T]) extends AbstractObservable[T] {
  private var setIntervalHandle = Option.empty[SetIntervalHandle]
  override def onStart(): Unit  = {
    val si = interval match {
      case i: Double         => setInterval(i)
      case i: FiniteDuration => setInterval(i)
    }
    setIntervalHandle = Some(si {
      if (i.hasNext) {
        subscribers.foreach(_.onNext(i.next()))
      } else {
        subscribers.foreach(_.onCompleted)
        onStop()
      }
    })
  }

  override def onStop(): Unit = setIntervalHandle.foreach(clearInterval)
}
