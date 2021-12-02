import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day22 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day22.txt")) {
      src => {
        val ll = src.getLines.toList
        val player1 = ll.tail.takeWhile(_.nonEmpty).map(_.toInt)
        val player2 = ll.drop(player1.size + 3).map(_.toInt)

        println(getPlayerScore(getWinnerHand(player1, player2)))
        println(getPlayerScore(getWinnerHandWithSubgames(player1, player2)))
      }
    }
  }

  def getPlayerScore(p: List[Int]): Int = p.reverse.zipWithIndex.foldLeft(0) {
    case (total, (number, index)) => total + (number * (index + 1))
  }

  def getWinnerHandWithSubgames(p1: List[Int], p2: List[Int]): List[Int] = {
    def getGameWinner(p1: List[Int], p2: List[Int], seen: Set[String]): (Int, List[Int]) = {
      val round = s"p1: $p1, p2: $p2"
      if (p1.isEmpty) (2, p2)
      else if (p2.isEmpty || seen.contains(round)) (1, p1)
      else if (p1.head < p1.size && p2.head < p2.size) getGameWinner(p1.slice(1, p1.head + 1), p2.slice(1, p2.head + 1), Set()) match {
        case (1, _) => getGameWinner(p1.tail :+ p1.head :+ p2.head, p2.tail, seen + round)
        case _ => getGameWinner(p1.tail, p2.tail :+ p2.head :+ p1.head, seen + round)
      } else if (p1.head > p2.head) getGameWinner(p1.tail :+ p1.head :+ p2.head, p2.tail, seen + round)
      else getGameWinner(p1.tail, p2.tail :+ p2.head :+ p1.head, seen + round)
    }

    getGameWinner(p1, p2, Set()) match {
      case (_, deck) => deck
    }
  }

  @tailrec
  def getWinnerHand(p1: List[Int], p2: List[Int]): List[Int] = {
    if (p1.isEmpty) p2
    else if (p2.isEmpty) p1
    else if (p1.head > p2.head) getWinnerHand(p1.tail :+ p1.head :+ p2.head, p2.tail)
    else getWinnerHand(p1.tail, p2.tail :+ p2.head :+ p1.head)
  }
}
