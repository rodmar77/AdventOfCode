import scala.io.Source
import scala.util.Using

object Day15 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day15.txt")) {
      source =>
        val ll = source.getLines.reduce(_ ++ _).split(",").map(_.toInt).toList
        println(getNumber(ll, 2020))
        println(getNumber(ll, 30000000))
    }
  }

  def getNumber(ll: List[Int], idx: Int): Int = {
    def updateFor(m: Map[Int, List[Int]], key: Int, index: Int): Map[Int, List[Int]] = {
      if (m.contains(key)) m + (key -> List(m(key).last, index))
      else m + (key -> List(index))
    }

    def _getNumber(m: Map[Int, List[Int]], last: Int, ci: Int): Int = {
      if (ci == idx) last
      else if (m(last).length == 1) _getNumber(m + (0 -> List(m(0).last, ci)), 0, ci + 1)
      else {
        val number = ci - m(last).head - 1
        _getNumber(updateFor(m, number, ci), number, ci + 1)
      }
    }

    _getNumber(ll.zipWithIndex.map {
      case (n, i) => n -> List(i)
    }.toMap, ll.last, ll.length)
  }
}
