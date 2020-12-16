import scala.io.Source
import scala.util.Using

object Day09 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day09.txt")) {
      source =>
        val numbers = source.getLines.map(_.toLong).toList
        val missingNumber = numbers.drop(25).zipWithIndex.find {
          case (number, index) => !numbers.slice(index, index + 25).combinations(2).map(_.sum).contains(number)
        } match {
          case Some((number, _)) => number
        }

        println(missingNumber)

        val sublist = indexesFor(numbers, missingNumber) match {
          case (lo, hi) => numbers.slice(lo, hi)
        }

        println(sublist.min + sublist.max)
    }
  }

  def indexesFor(numbers: List[Long], missingNumber: Long) = {
    def indexesFor(lo: Int, hi: Int, acc: Long): (Int, Int) = {
      if (acc == missingNumber) (lo, hi)
      else if (acc < missingNumber) indexesFor(lo, hi + 1, acc + numbers(hi + 1))
      else indexesFor(lo + 1, hi, acc - numbers(lo))
    }

    indexesFor(0, 1, numbers.take(2).sum)
  }

}

