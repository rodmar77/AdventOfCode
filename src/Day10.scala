import scala.io.Source

object Day10 extends App {

  val first = Source.fromFile("inputs/input_day10.txt").getLines.mkString
  println(lookAndSay(first, 40).length)
  println(lookAndSay(first, 50).length)

  def lookAndSay(start: String, count: Int) = {
    val regex = """(\d)(\1*)""".r
    def lookAndSay(idx: Int, acc: String): String = {
      if (idx == count) acc
      else lookAndSay(
        idx + 1,
        regex
          .findAllMatchIn(acc)
          .map(m => m.group(0).length + m.group(1)).mkString)
    }

    lookAndSay(0, start)
  }

}