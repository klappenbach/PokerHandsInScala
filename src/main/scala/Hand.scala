import collection.immutable.SortedSet

case class Hand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  val cards = try {
    SortedSet(card1, card2, card3, card4, card5).toList
  } catch {
    case e: NullPointerException => throw new CardIsNullException
  }
  if (cards.size != 5) throw new DuplicateCardsException

  lazy val highCard = cards.reverse.head

  lazy val isPair = cards.withoutPairs.withoutThreeOfAKind.size == 3

  lazy val isTwoPair = cards.withoutPairs.size == 1

  lazy val isStraight = cards.map(_.rank.id).toStream zip Stream.from(cards.head.rank.id) forall {
    case (a, b) => a == b
  }

  lazy val isFullHouse = cards.withoutPairs.withoutThreeOfAKind.size == 0

  lazy val isFlush = cards.forall(card => card.suite.id == cards.head.suite.id )

  lazy val isStraightFlush = isStraight && isFlush

  lazy val isFourOfAKind = cards.withoutFourOfAKind.size == 1  // 4 cards -> 1 since they are of equal rank

  lazy val isThreeOfAKind = cards.withoutThreeOfAKind.withoutPairs.size == 2 // withoutPairs should not change the result (not full house)

  trait CardList {
    val cards: List[Card]

    def withoutPairs = withoutRankGroupsOfSize(2)
    def withoutThreeOfAKind = withoutRankGroupsOfSize(3)
    def withoutFourOfAKind = withoutRankGroupsOfSize(4)

    def withoutRankGroupsOfSize(groupSize: Int): List[Card] = {
        val groupedByRank = cards.groupBy(_.rank.id).values
        val withoutGroupOfSize = groupedByRank.filterNot(_.size == groupSize)
        withoutGroupOfSize.flatten.toList
      }
  }

  implicit def toCardList(list: List[Card]): CardList = new CardList{ val cards = list}
}



class InvalidHandException(msg:String) extends RuntimeException(msg)
class CardIsNullException extends InvalidHandException("No card in a hand may be null")
class DuplicateCardsException extends InvalidHandException("Duplicates cards are not allowed")