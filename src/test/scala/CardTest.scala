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

  test("Three of hearts is sorted neither before nor after three of spades") {
    val threeOfSpades = Card(THREE, SPADES)
    val threeOfHearts = Card(THREE, HEARTS)
    
    threeOfHearts must {
      not be > (threeOfSpades) and not be < (threeOfSpades)
    }
  }
}

