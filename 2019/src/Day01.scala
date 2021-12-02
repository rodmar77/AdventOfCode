import scala.io.Source
import scala.util.Using

object Day01 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day01.txt")) {
      source =>
        val numbers = source.getLines.map(_.toInt).toList
        println(numbers.map(_ / 3 - 2).sum)
        println(numbers.map(requiredFuel).sum)
    }
  }

  def requiredFuel(n: Int): Int = {
    val x = n / 3 - 2
    if (x <= 0) 0 else x + requiredFuel(x)
  }
}
