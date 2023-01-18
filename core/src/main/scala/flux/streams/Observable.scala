package flux.streams

import flux.streams.constructor.{FutureObservable, IterableObservable}
import flux.streams.operators.{DropOperator, FilterOperator, FoldOperator, MapOperator}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait Observable[+T] {
  def subscribe[S >: T](subscriber: Subscriber[S]): Unit
  def unsubscribe[S >: T](subscriber: Subscriber[S]): Unit

  def filter(f: T => Boolean): Observable[T]       = FilterOperator(this, f)
  def map[U](f: T => U): Observable[U]             = MapOperator(this, f)
  def fold[U](z: U)(f: (U, T) => U): Observable[U] = FoldOperator(this, z, f)
  def drop(n: Int): Observable[T]                  = DropOperator(this, n)
}

object Observable {
  def from[T](t: T*): Observable[T]                                            = from(t)
  def from[T](t: Iterable[T]): Observable[T]                                   = IterableObservable(t)
  def from[T](f: Future[T])(implicit ec: ExecutionContext): Observable[Try[T]] = FutureObservable(f)
}
