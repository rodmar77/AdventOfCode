import scala.io.Source
import scala.language.implicitConversions
import scala.util.Using

object Day23 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day23.txt")) {
      source =>
        val burrow = source.getLines().toList
        println(findMinimumCost(burrow))
    }
  }

  def findMinimumCost(burrow: List[String]) = {
    burrow
      .updated(1, burrow(1).indices.map(idx => if (burrow(1)(idx) == '.' && burrow(2)(idx) == '#') '.' else '#').mkString)
  }

}
