import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Suite._
import Rank._

class HandTest extends FunSuite with ShouldMatchers {
  val threeOfSpades = Card(THREE, SPADES)
  val threeOfHearts = Card(THREE, HEARTS)
  val threeOfClubs = Card(THREE, CLUBS)
  val threeOfDiamonds = Card(THREE, DIAMONDS)
  val twoOfSpades = Card(TWO, SPADES)

  test("A hand can be instantiated") {
    Hand(threeOfSpades, threeOfDiamonds, threeOfClubs, threeOfHearts, twoOfSpades)
  }

  test("Duplicate card makes hand invalid") {
    intercept[InvalidHandException] {
      Hand(threeOfSpades, threeOfDiamonds, threeOfClubs, threeOfHearts, threeOfHearts)
    }
  }

  test("A card that is null in a hand makes the hand invalid") {
    intercept[InvalidHandException] {
      Hand(threeOfSpades, threeOfDiamonds, threeOfClubs, threeOfHearts, null)
    }
  }

  test("highcard is six of hearts, in a hand where the highest card is six of hearts") {
    val hand = Hand(Card(TWO, HEARTS), Card(THREE, HEARTS), Card(FIVE, SPADES), Card(TWO, SPADES),  Card(SIX, HEARTS))
    hand.highCard should be (Card(SIX, HEARTS))
  }
}