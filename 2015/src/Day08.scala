import scala.io._

object Day08 extends App {

  val lines = Source.fromFile("inputs/2015/input_day08.txt").getLines.toList

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
    def escapeChar(c: Char) = if (c == '"' || c == '\\') s"\\$c" else s"$c"
    '"' + raw.map(escapeChar).mkString + '"'
  }
}