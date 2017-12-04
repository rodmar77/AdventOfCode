import scala.io.Source

object Day06 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("inputs/2016/input_day06.txt").getLines.toList

    println(
      lines
        .transpose
        .map(_.mkString)
        .map(_
          .groupBy(_.toChar)
          .map(p => (p._1, p._2.length))
          .maxBy(_._2)
          ._1)
        .mkString)

    println(
      lines
        .transpose
        .map(_.mkString)
        .map(_
          .groupBy(_.toChar)
          .map(p => (p._1, p._2.length))
          .minBy(_._2)
          ._1)
        .mkString)
  }
}
