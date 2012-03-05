import org.scalatest.FunSuite
import Suite._
import Rank._

class HandTest extends FunSuite {
  val threeOfSpades = Card(THREE, SPADES)
  val threeOfHearts = Card(THREE, HEARTS)
  val threeOfClubs = Card(THREE, CLUBS)
  val threeOfDiamonds = Card(THREE, DIAMONDS)
  val twoOfSpades = Card(TWO, SPADES)

  test("A hand can be instantiated") {
    Hand(threeOfSpades, threeOfDiamonds, threeOfClubs, threeOfHearts, twoOfSpades)
  }
}