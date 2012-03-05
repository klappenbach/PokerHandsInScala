import collection.immutable.SortedSet

case class Hand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  val cards = Set(card1, card2, card3, card4, card5)
  if (cards.filter(_ != null).size != 5) throw new InvalidHandException

  def highCard = cards.head
}

class InvalidHandException extends RuntimeException {}