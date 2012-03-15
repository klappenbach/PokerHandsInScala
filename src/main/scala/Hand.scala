import collection.immutable.SortedSet

case class Hand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  val cards = try {
    SortedSet(card1, card2, card3, card4, card5).toList
  } catch {
    case e: NullPointerException => throw new CardIsNullException
  }
  if (cards.size != 5) throw new DuplicateCardsException

  lazy val highCard = cards.reverse.head

  lazy val isPair = cards.subtractPairs.subtractThreeOfAKind.size == 3

  lazy val isTwoPair = cards.subtractPairs.size == 1

  lazy val isThreeOfAKind = cards.subtractThreeOfAKind.subtractPairs.size == 2 // subtractPairs should not change the result (not full house)

  lazy val isFullHouse = cards.subtractPairs.subtractThreeOfAKind.size == 0

  lazy val isFourOfAKind = cards.subtractFourOfAKind.size == 1

  lazy val isFlush = cards.forall(card => card.suite.id == cards.head.suite.id )

  lazy val isStraight = cards.map(_.rank.id).toStream zip Stream.from(cards.head.rank.id) forall {
      case (a, b) => a == b
  }

  lazy val isStraightFlush = isStraight && isFlush

  implicit def toCardList(list: List[Card]): CardList = new CardList{ val cards = list}

  trait CardList {
    val cards: List[Card]

    def subtractPairs = subtractRankGroupsOfSize(2)
    def subtractThreeOfAKind = subtractRankGroupsOfSize(3)
    def subtractFourOfAKind = subtractRankGroupsOfSize(4)

    private def subtractRankGroupsOfSize(groupSize: Int): List[Card] = {
        val groupedByRank = cards.groupBy(_.rank.id).values
        val withoutGroupOfSize = groupedByRank.filterNot(_.size == groupSize)
        withoutGroupOfSize.flatten.toList
      }
  }

}



class InvalidHandException(msg:String) extends RuntimeException(msg)
class CardIsNullException extends InvalidHandException("No card in a hand may be null")
class DuplicateCardsException extends InvalidHandException("Duplicates cards are not allowed")