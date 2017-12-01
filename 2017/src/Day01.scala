import scala.io.Source

object Day01 {

  def main(args: Array[String]): Unit = {
    val s = Source.fromFile("inputs/2017/input_day01.txt").getLines.mkString
    println(
      (s + s.head)
        .sliding(2)
        .map(mapper)
        .sum)

    println(
      (s + s.substring(0, s.length/2))
        .sliding(s.length / 2 + 1)
        .map(mapper)
        .sum)
  }

  def mapper(s: String): Int = if (s.head == s.last) s.head - '0' else 0

}
