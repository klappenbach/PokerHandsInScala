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
    val hand = Hand(Card(TWO, HEARTS), Card(THREE, HEARTS), Card(FIVE, SPADES), Card(TWO, SPADES), Card(SIX, HEARTS))
    hand.highCard should be(Card(SIX, HEARTS))
  }

  test("2 3 4 5 6 in different suits forms a straight") {
    val hand = Hand(Card(TWO, HEARTS), Card(THREE, HEARTS), Card(FOUR, SPADES), Card(FIVE, SPADES), Card(SIX, HEARTS))
    hand.isStraight should be(true)
  }

  test("2 3 4 5 ace in different suits does not form any hand") {
    val hand = Hand(Card(TWO, HEARTS), Card(THREE, HEARTS), Card(FOUR, SPADES), Card(FIVE, SPADES), Card(ACE, HEARTS))
    hand.isStraight should be(false)
    hand.isPair should be(false)
    hand.isTwoPair should be(false)
    hand.isThreeOfAKind should be(false)
    hand.isFourOfAKind should be(false)
    hand.isFlush should be(false)
    hand.isStraightFlush should be(false)
   hand.isFullHouse should be(false)
  }

  test("2 3 4 6 ace of clubs forms a flush") {
    val hand = Hand(Card(TWO, CLUBS), Card(THREE, CLUBS), Card(FOUR, CLUBS), Card(SIX, CLUBS), Card(ACE, CLUBS))
    hand.isFlush should be(true)
  }

  test("2 of spades and 3 4 6 ace of clubs does not form a flush") {
    val hand = Hand(Card(TWO, SPADES), Card(THREE, CLUBS), Card(FOUR, CLUBS), Card(SIX, CLUBS), Card(ACE, CLUBS))
    hand.isFlush should be(false)
  }

  test("2 3 4 5 6 of diamonds forms a straight flush") {
    val hand = Hand(Card(TWO, DIAMONDS), Card(THREE, DIAMONDS), Card(FOUR, DIAMONDS), Card(FIVE, DIAMONDS), Card(SIX, DIAMONDS))
    hand.isStraightFlush should be(true)
  }

  test("5 5 5 5 6 of different suits forms four of a kind") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(FIVE, DIAMONDS), Card(ACE, HEARTS))
    hand.isFourOfAKind should be(true)
  }

  test("5 5 5 2 6 of different suits does not form four of a kind") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(TWO, DIAMONDS), Card(ACE, HEARTS))
    hand.isFourOfAKind should be(false)
  }

  test("5 5 5 2 2 of different suits does not form four of a kind") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(TWO, DIAMONDS), Card(TWO, HEARTS))
    hand.isFourOfAKind should be(false)
  }

  test("5 5 5 2 6 of different suits forms three of a kind") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(TWO, DIAMONDS), Card(ACE, HEARTS))
    hand.isThreeOfAKind should be(true)
  }

  test("5 5 5 5 6 of different suits does not form three of a kind") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(FIVE, DIAMONDS), Card(ACE, HEARTS))
    hand.isThreeOfAKind should be(false)
  }

  test("5 5 2 3 6 of different suits forms  a pair") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(TWO, SPADES), Card(THREE, DIAMONDS), Card(ACE, HEARTS))
    hand.isPair should be(true)
  }

  test("5 5 5 3 6 of different suits does not form  a pair") {
    val hand = Hand(Card(FIVE, HEARTS), Card(FIVE, CLUBS), Card(FIVE, SPADES), Card(THREE, DIAMONDS), Card(ACE, HEARTS))
    hand.isPair should be(false)
  }
}