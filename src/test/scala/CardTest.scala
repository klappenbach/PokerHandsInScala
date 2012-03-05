import org.scalatest.FunSuite;
import org.scalatest.matchers.MustMatchers
import Rank._
import Suite._

class CardTest extends FunSuite with MustMatchers {

  test("Card two of hearts can be instantiated") {
    Card(TWO, HEARTS)
  }

  test("A card with rank three is higher than a card of rank two") {
    Card(THREE, HEARTS) must be > Card(TWO, HEARTS)
  }

  test("Three of hearts is neither higher nor lower than three of hearts") {
    val threeOfHearts = Card(THREE, HEARTS)

    threeOfHearts must {
      not be > (threeOfHearts) and not be < (threeOfHearts)
    }
  }

  test("Four of hearts does not equal four of spades") {
    Card(FOUR, HEARTS) must not equal Card(FOUR, SPADES)
  }

  test("Four of hearts equals four of hearts") {
    Card(FOUR, HEARTS) must equal(Card(FOUR, HEARTS))
  }
}

