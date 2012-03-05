import org.scalatest.FunSuite;
import Rank._
import Suite._

class CardTest extends FunSuite {

  test("A card can be instantiated") {
    Card(ONE, HEARTS)
  }
}

