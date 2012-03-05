import Rank._
import Suite._

case class Card(rank: Rank, suite: Suite) extends Ordered[Card] {

  /**
   * Compares rank first, and if same rank compare suite.
   */
  def compare(that: Card) = rank compare that.rank match {
    case 0 => suite.compare(that.suite)
    case x => x
  }
}
