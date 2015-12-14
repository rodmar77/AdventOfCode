import scala.io.Source

object Day12 extends App {

  val text = Source
    .fromFile("/tmp/input.txt")
    .getLines
    .mkString

  println(sumNumbers(text))
  println(secondPart(text))

  def sumNumbers(text: String) =
    """(-?\d+)"""
      .r
      .findAllMatchIn(
        text
          .replaceAll( """"[^"]+"""", ""))
      .map(_.group(0).toInt)
      .sum

  def secondPart(text: String) = {

  }

}
