import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21 {

  private val playerRegex = """Player \d starting position: (\d)""".r
  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day21.txt")) {
      source =>
        val positions = source.getLines().map {
          case playerRegex(position) => position.toInt
        }.toList

        getGameData(positions) match {
          case (score, rolls) => println(score * rolls)
        }

        println(getGameDataWithDirac(positions) match {
          case (a, b) => a max b
        })
    }
  }

  def getGameDataWithDirac(positions: List[Int]): (BigInt, BigInt) = {
    val cache = mutable.Map[(Int, ((Int, Int), (Int, Int))), (BigInt, BigInt)]()
    val s = List(1, 2, 3)
    val diracSums = s.flatMap(a => s.flatMap(b => s.map(a + b + _)))

    def getGameDataWithDirac(player: Int, data: ((Int, Int), (Int, Int))): (BigInt, BigInt) = {
      cache.getOrElseUpdate((player, data), {
        data match {
          case ((p1, s1), (p2, s2)) =>
            if (s1 >= 21) (1, 0)
            else if (s2 >= 21) (0, 1)
            else diracSums
              .map(rolled =>
                  if (player == 1) {
                    val nextPosition = if (rolled + p1 > 10) (rolled + p1) % 10 else rolled + p1
                    getGameDataWithDirac(2, ((nextPosition, s1 + nextPosition), (p2, s2)))
                  } else {
                    val nextPosition = if (rolled + p2 > 10) (rolled + p2) % 10 else rolled + p2
                    getGameDataWithDirac(1, ((p1, s1), (nextPosition, s2 + nextPosition)))
                  }
              ).reduce[(BigInt, BigInt)] {
              case ((a1, b1), (a2, b2)) => (a1 + a2, b1 + b2)
            }
        }
      })
    }

    getGameDataWithDirac(1, ((positions.head, 0), (positions.last, 0)))
  }

  def getGameData(positions: List[Int]): (Int, Int) = {
    def diceData(diePosition: Int) = ((diePosition%100 + (diePosition+1)%100 + (diePosition+2)%100) % 10, (diePosition+3)%100)
    @tailrec
    def getGameData(diePosition: Int, data: ((Int, Int), (Int, Int), Int)): (Int, Int) = data match {
      case ((cp, cs), (np, ns), r) =>
        if (ns >= 1000) (cs, r)
        else diceData(diePosition) match {
          case (rolled, nextDiePosition) =>
            val nextPosition = if (rolled + cp > 10) (rolled + cp) % 10 else rolled + cp
            getGameData(nextDiePosition, ((np, ns), (nextPosition, cs + nextPosition), r+3))
        }
    }

    getGameData(1, ((positions.head, 0), (positions.last, 0), 0))
  }

}
