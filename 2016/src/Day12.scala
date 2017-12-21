import scala.io.Source

object Day12 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day12.txt")
      .getLines
      .toList

    println(execute(commands)("a"))
    println(execute(commands, Map("c" -> 1))("a"))
  }

  def execute(commands: List[String], regs: Map[String, Int] = Map()) = {
    val (cpy, inc, dec, jnz) = (
      "cpy ([+-]?[a-z0-9]+) (\\w+)".r,
      "inc (\\w+)".r,
      "dec (\\w+)".r,
      "jnz (\\w+) ([+-]?\\d+)".r
    )

    def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    def isZero(x: String, regs: Map[String, Int]) = {
      if (isNumeric(x)) x.toInt == 0
      else regs(x) == 0
    }

    def execute(idx: Int, regs: Map[String, Int]): Map[String, Int] = {
      if (idx >= commands.size) regs
      else commands(idx) match {
        case cpy(x, y) => execute(idx + 1, regs + (if (isNumeric(x)) y -> x.toInt else y -> regs(x)))
        case inc(x) => execute(idx + 1, regs + (x -> (regs(x) + 1)))
        case dec(x) => execute(idx + 1, regs + (x -> (regs(x) - 1)))
        case jnz(x, y) => if (isZero(x, regs)) execute(idx + 1, regs) else execute(idx + y.toInt, regs)
      }
    }

    execute(0, regs.withDefaultValue(0))
  }
}
