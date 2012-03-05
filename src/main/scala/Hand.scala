import collection.immutable.SortedSet

case class Hand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  val cards = try {
    SortedSet(card1, card2, card3, card4, card5).toList.reverse
  } catch {
    case e: NullPointerException => throw new CardIsNullException
  }
  if (cards.size != 5) throw new DuplicateCardsException

  def highCard = cards.head
}

class InvalidHandException(msg:String) extends RuntimeException(msg)
class CardIsNullException extends InvalidHandException("No card in a hand may be null")
class DuplicateCardsException extends InvalidHandException("Duplicates cards are not allowed")