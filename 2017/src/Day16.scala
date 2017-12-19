import scala.io.Source

object Day16 {

  def main(args: Array[String]): Unit = {
    val dances = Source
      .fromFile("inputs/2017/input_day16.txt")
      .mkString
      .split(",")
      .toList

    println(fold(dances, 1))
    println(fold(dances, 1000000000))
  }

  def fold(ll: List[String], count: Int) = {
    val (spin, exchange, partner) = ("s(\\d+)".r, "x(\\d+)/(\\d+)".r, "p(\\w+)/(\\w+)".r)
    def applyCommand(program: String, command: String): String = command match {
      case spin(x) => program.drop(program.length - x.toInt) + program.take(program.length - x.toInt)
      case exchange(a, b) => program.updated(a.toInt, program.charAt(b.toInt)).updated(b.toInt, program.charAt(a.toInt))
      case partner(a, b) => program.updated(program.indexOf(a), b.head).updated(program.indexOf(b), a.head)
    }

    def fold(acc: String, rep: Int): String = {
      if (rep == count % 63) acc
      else fold(ll.foldLeft(acc)(applyCommand), rep + 1)
    }

    fold("abcdefghijklmnop", 0)
  }

}
