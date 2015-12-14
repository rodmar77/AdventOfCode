import scala.io._

object Day8 extends App {

  val lines = Source.fromFile("inputs/input_day08.txt").getLines.toList

  println(
    lines.map(_.length).sum -
      lines.map(t => """\\x[0-9a-f]{2}""".r
        .replaceAllIn(t, "'")
        .replace("\\\\", "$")
        .replace("\\\"", "$")
        .length - 2).sum)

  println(
    lines.map(escape(_).length).sum -
      lines.map(_.length).sum)

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }
}