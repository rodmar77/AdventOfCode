import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day24 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day24.txt")) {
      src => {
          val lines = src.getLines.toList
          val coords = lines
                          .map(toCoord)
                          .groupBy(p => p)
                          .view
                          .mapValues(_.size)
                          .filterNot {
                            case (_, size) => size % 2 == 0
                          }
                          .map {
                            case (k, _) => k
                          }
                          .toList

          println(coords.size)
          println(applyRules(coords, 100).size)
      }
    }
  }

  @tailrec
  def applyRules(ll: List[(Int, Int)], dayCount: Int): List[(Int, Int)] = {
    def adjacencyCount(p: (Int, Int)) = adjacency(p).count(ll.contains(_))
    def adjacency(p: (Int, Int)) = p match {
      case (x, y) => y % 2 match {
        case 0 => List((x-1, y+1), (x  , y+1), (x-1, y-1), (x  , y-1), (x+1, y), (x-1, y))
        case _ => List((x  , y+1), (x+1, y+1), (x  , y-1), (x+1, y-1), (x+1, y), (x-1, y))
      }
    }

    def blackToWhite() = ll.filterNot((1 to 2) contains adjacencyCount(_)).toSet
    def whiteToBlack() = ll.flatMap(adjacency).distinct.filterNot(ll.toSet).filter(adjacencyCount(_) == 2)

    if (dayCount == 0) ll
    else applyRules(ll.filterNot(blackToWhite()) ++ whiteToBlack(), dayCount - 1)
  }

  def toCoord(line: String): (Int, Int) = {
    @tailrec
    def toCoord(l: String, acc: (Int, Int)): (Int, Int) = {
      if (l.isEmpty) acc
      else acc match {
        case (x, y) => l.head match {
          case 'e' => toCoord(l.tail, (x + 1, y))
          case 'w' => toCoord(l.tail, (x - 1, y))
          case _ => l.take(2) match {
            case "sw" => toCoord(l.drop(2), if (y % 2 == 0) (x-1, y+1) else (x  , y+1))
            case "se" => toCoord(l.drop(2), if (y % 2 == 0) (x  , y+1) else (x+1, y+1))
            case "nw" => toCoord(l.drop(2), if (y % 2 == 0) (x-1, y-1) else (x  , y-1))
            case _    => toCoord(l.drop(2), if (y % 2 == 0) (x  , y-1) else (x+1, y-1))
          }
        }
      }
    }

    toCoord(line, (0, 0))
  }
}
