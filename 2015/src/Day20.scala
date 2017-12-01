import java.lang.Math.sqrt

import scala.io.Source

object Day20 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day20.txt")
    .getLines
    .mkString
    .toInt

  println(Range(1, Int.MaxValue).dropWhile(sumMultiples(_)*10 <= data).head)
  println(Range(1, Int.MaxValue).dropWhile(n => sumMultiples(n)(_ * 50d >= n)*11 <= data).head)

  def sumMultiples(n: Int)(implicit f: Int => Boolean = (_ => true)) = (1 to sqrt(n).toInt)
    .filter(n % _ == 0)
    .flatMap(x => Seq(x, n/x))
    .filter(f)
    .distinct
    .sum
}
