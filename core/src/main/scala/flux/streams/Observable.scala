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

  def filter(f: T => Boolean): Observable[T]                                              = FilterOperator(this, f)
  def map[U](f: T => U): Observable[U]                                                    = MapOperator(this, f)
  def mapTo[U](to: U): Observable[U]                                                      = MapOperator(this, _ => to)
  def text(): Observable[String]                                                          = MapOperator(this, _.toString)
  def fold[U](z: U)(f: (U, T) => U): Observable[U]                                        = FoldOperator(this, z, f)
  def drop(n: Int): Observable[T]                                                         = DropOperator(this, n)
  def merge[T2](o2: Observable[T2]): Observable[T | T2]                                   = MergeOperator(Iterable(this, o2))
  def remember(): Observable[T]                                                           = RememberOperator(this)
  def tap(f: T => Unit): Observable[T]                                                    = TapOperator(this, f)
  def startWith[U >: T](start: U): Observable[U]                                          = StartWithOperator(this, start)
  def combine[T2](o2: Observable[T2]): Observable[(T, T2)]                                = CombineOperator(this, o2)
  def dropRepeats(isEqual: (T, T) => Boolean = (t1: T, t2: T) => t1 == t2): Observable[T] = DropRepeatsOperator(this, isEqual)

  def subscribeNext(f: T => Unit) = this.subscribe(new Subscriber[T] {
    override def onNext(t: T): Unit = f(t)

    override def onCompleted: Unit = {}
  })
}

object Observable {
  def once[T](t: T): Observable[T]                                                       = IterableObservable(Iterable(t))
  def from[T](t: T*): Observable[T]                                                      = from(t)
  def from[T](t: Iterable[T]): Observable[T]                                             = IterableObservable(t)
  def from[T](f: Future[T])(implicit ec: ExecutionContext): Observable[Try[T]]           = FutureObservable(f)
  def fromEventListener[T <: Event](element: EventTarget, `type`: String): Observable[T] = EventListenerObservable[T](element, `type`)
  def periodic[T](interval: Interval)(i: Iterator[T]): Observable[T]                     = PeriodicObservable(interval, i)
  def periodic(interval: Interval): Observable[Int]                                      = PeriodicObservable(interval, Iterator.from(0))
  def merge[T1, T2](o1: Observable[T1], o2: Observable[T2]): Observable[T1 | T2]         = MergeOperator(Iterable(o1, o2))
  def merge[T](observables: Observable[T]*): Observable[T]                               = MergeOperator(observables)
  def combine[T1, T2](o1: Observable[T1], o2: Observable[T2]): Observable[(T1, T2)]      = CombineOperator(o1, o2)
}
