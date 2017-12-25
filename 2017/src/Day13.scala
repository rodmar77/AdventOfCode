import scala.io.Source

object Day13 {

  def main(args: Array[String]): Unit = {
    val data = Source
      .fromFile("inputs/2017/input_day13.txt")
      .getLines
      .map(_
        .split(": ")
        .map(_.toInt))
      .map(arr => (arr.head, (arr.last - 1) * 2, arr.last))
      .toList

    println(data
      .filter(p => p._1 % p._2 == 0)
      .map(p => p._1 * p._3)
      .sum)

    println(
      (1 to Int.MaxValue)
        .find(k => !data
          .exists(p =>
            (p._1 + k) % p._2 == 0))
        .get)
  }

}
