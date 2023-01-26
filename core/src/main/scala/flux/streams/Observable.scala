package flux.streams

import flux.streams.constructor.*
import flux.streams.operators.*
import org.scalajs.dom.{Event, EventTarget}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait Observable[+T] {
  def subscribe[S >: T](subscriber: Subscriber[S]): Subscription
  def unsubscribe[S >: T](subscriber: Subscriber[S]): Unit
  def debug: String

  def filter(f: T => Boolean): Observable[T]            = FilterOperator(this, f)
  def map[U](f: T => U): Observable[U]                  = MapOperator(this, f)
  def mapTo[U](to: U): Observable[U]                    = MapOperator(this, _ => to)
  def fold[U](z: U)(f: (U, T) => U): Observable[U]      = FoldOperator(this, z, f)
  def drop(n: Int): Observable[T]                       = DropOperator(this, n)
  def merge[T2](o2: Observable[T2]): Observable[T | T2] = MergeOperator(this, o2)
  def remember(): Observable[T]                         = RememberOperator(this)
  def startWith[U >: T](start: U): Observable[U]        = StartWithOperator(this, start)
}

object Observable {
  def from[T](t: T*): Observable[T]                                                      = from(t)
  def from[T](t: Iterable[T]): Observable[T]                                             = IterableObservable(t)
  def from[T](f: Future[T])(implicit ec: ExecutionContext): Observable[Try[T]]           = FutureObservable(f)
  def fromEventListener[T <: Event](element: EventTarget, `type`: String): Observable[T] = EventListenerObservable[T](element, `type`)
  def periodic[T](interval: Interval)(i: Iterator[T]): Observable[T]                     = PeriodicObservable(interval, i)
  def periodic(interval: Interval): Observable[Int]                                      = PeriodicObservable(interval, Iterator.from(0))
  def merge[T1, T2](o1: Observable[T1], o2: Observable[T2]): Observable[T1 | T2]         = MergeOperator(o1, o2)
  def merge[T](observables: Observable[T]*): Observable[T]                               = MergeAllOperator(observables)
}
