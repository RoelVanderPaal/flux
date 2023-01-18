package flux.streams.constructor

import flux.streams.Subscriber

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class FutureObservable[T](f: Future[T])(implicit ec: ExecutionContext) extends AbstractObservable[Try[T]] {
  override def onStart(): Unit = f.onComplete(t => {
    subscribers.foreach(_.onNext(t))
    subscribers.foreach(_.onCompleted)
  })
}
