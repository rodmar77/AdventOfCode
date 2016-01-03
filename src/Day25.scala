import scala.io.Source

object Day25 extends App {

  val pattern = """.+?(\d+).+?(\d+).""".r

  def colValue(line: Int, col: Int): BigInt = {
    def lineStart(line: Int): Int = {
      if (line == 1) 1
      else (line - 1) + lineStart(line - 1)
    }

    def colValue(curr: Int, acc: Int): Int = {
      if (curr == col) acc
      else colValue(curr + 1, acc + line + curr)
    }

    val m = colValue(1, lineStart(line)) - 1
    val k = BigInt(252533).modPow(m, 33554393)
    (20151125 * k) % 33554393
  }

  val data = Source
    .fromFile("inputs/input_day25.txt")
    .getLines
    .map {
      case pattern(line, col) => (line.toInt, col.toInt)
    }
    .toList
    .head

  println(colValue(data._1, data._2))

}