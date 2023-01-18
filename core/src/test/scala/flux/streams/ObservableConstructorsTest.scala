package flux.streams

import flux.streams.util.{ListSubscriber, TestBase}

import scala.concurrent.Future
import scala.util.{Success, Try}

class ObservableConstructorsTest extends TestBase {
  test("vararg") {
    check(Observable.from(1, 2, 3), Seq(1, 2, 3))
  }
  test("iterable") {
    check(Observable.from(List(1, 2, 3)), Seq(1, 2, 3))
  }
  test("future success") {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val listener                                       = new ListSubscriber[Try[Int]]
    val future                                         = Future.successful(1)
    Observable.from(future).subscribe(listener)
    future.foreach(_ => {
      listener.items shouldBe List(Success(1))
      listener.completed shouldBe true
    })
  }
  test("future failure") {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val listener                                       = new ListSubscriber[Try[Int]]
    val future                                         = Future.failed(new RuntimeException("failed"))
    Observable.from(future).subscribe(listener)
    future.foreach(_ => {
      listener.items.head.isFailure shouldBe true
      listener.completed shouldBe true
    })
  }

}
