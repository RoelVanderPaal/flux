package flux.streams

trait Subscription {
  def unsubscribe(): Unit
}
