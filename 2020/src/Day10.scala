import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day10 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day10.txt")) {
      source =>
        val numbers = source.getLines.map(_.toInt).toList.sorted
        val m = (0 +: numbers :+ numbers.max + 3)
          .sliding(2)
          .map {
            case List(a, b) => b - a
          }
          .toList
          .groupBy(n => n)
          .view
          .mapValues(_.size)
          .toMap

        println(m(1) * m(3))
        println(countCombinations(numbers))
    }
  }

  def countCombinations(numbers: List[Int]): BigInt = {
    def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
      override def apply(key: I) = getOrElseUpdate(key, f(key))
    }

    val x = numbers.size
    lazy val combinations: ((Int, Int)) => BigInt = memoize {
      case (_, `x`) => 1
      case (current, si) => numbers
                              .zipWithIndex
                              .dropWhile {
                                case (_, index) => index < si
                              }
                              .filter {
                                case (number, _) => number - current <= 3
                              }
                              .map {
                                case (number, index) => combinations(number, index + 1)
                              }.sum
    }

    combinations(0, 0)
  }
}
