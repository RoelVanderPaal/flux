package flux.streams.util

import flux.streams.Observable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait TestBase extends AnyFunSuite with Matchers {
  def check[T](o: Observable[T], results: Seq[T]): Unit = {
    val subscriber   = ListSubscriber[T]()
    val subscription = o.subscribe(subscriber)
    subscriber.completed shouldBe true
    subscriber.items shouldBe results
    subscription.unsubscribe()
  }
}
