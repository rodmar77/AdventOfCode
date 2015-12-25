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

    def total(curr: Int, target: Int, acc: BigInt): BigInt = {
      if (curr == target) acc
      else total(curr + 1, target, (acc * 252533) % 33554393)
    }

    total(1, colValue(1, lineStart(line)), 20151125)
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