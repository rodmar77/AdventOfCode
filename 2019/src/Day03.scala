import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day03.txt")) {
      source =>
        val lines = source.getLines.map(_.split(",").toList).toList
        val positions = lines.map(toPositions)
        val crossings = positions match {
          case List(a, b) =>
            val ra = a.map { case (p, _) => p }
            val rb = b.map { case (p, _) => p }
            ra.intersect(rb).tail
        }

        println(crossings.map(manhattanDistance).min)

        val filteredPositions = positions
          .flatMap(filterByIntersection(_, crossings))
          .groupBy(position)
          .values
          .filter(containsTwoPositions)
          .map(toSumOfIndexes)

        println(filteredPositions.min)
    }
  }

  def manhattanDistance(p: (Int, Int)): Int = p match {
    case (x, y) => x.abs + y.abs
  }

  def toSumOfIndexes(list: List[((Int, Int), Int)]): Int = list.map {
    case (_, idx) => idx
  }.sum

  def containsTwoPositions(m: List[((Int, Int), Int)]): Boolean = m.length == 2

  def position(l: ((Int, Int), Int)): (Int, Int) = l match {
    case (p, _) => p
  }

  def filterByIntersection(l: List[((Int, Int), Int)], i: List[(Int, Int)]): List[((Int, Int), Int)] = l.filter {
    case (p, _) => i.contains(p)
  }

  def toPositions(ll: List[String]): List[((Int, Int), Int)] = {
    @tailrec
    def toPositions(xs: List[String], acc: List[((Int, Int), Int)]): List[((Int, Int), Int)] = {
      val pattern = "([UDLR])(\\d+)".r
      if (xs.isEmpty) acc
      else xs.head match {
        case pattern(direction, value) => (acc.last, direction) match {
          case (((x, y), idx), "U") => toPositions(xs.tail, acc ++ (1 to value.toInt).map(n => ((x,y+n),idx+n)))
          case (((x, y), idx), "D") => toPositions(xs.tail, acc ++ (1 to value.toInt).map(n => ((x,y-n),idx+n)))
          case (((x, y), idx), "L") => toPositions(xs.tail, acc ++ (1 to value.toInt).map(n => ((x-n,y),idx+n)))
          case (((x, y), idx), "R") => toPositions(xs.tail, acc ++ (1 to value.toInt).map(n => ((x+n,y),idx+n)))
        }
      }
    }

    toPositions(ll, List(((0, 0), 0)))
  }
}
