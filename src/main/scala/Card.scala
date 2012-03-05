import Rank._
import Suite._

case class Card(rank: Rank, suite: Suite) extends Ordered[Card] {
  def compare(that: Card) = if (rank == that.rank) 0 else if (rank > that.rank) 1 else -1
}
