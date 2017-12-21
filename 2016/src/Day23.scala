import scala.io.Source

object Day23 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day23.txt")
      .getLines
      .toList

    println(execute(commands, Map("a" -> 7))("a"))
  }

  def execute(commands: List[String], regs: Map[String, Int] = Map()) = {
    val (cpy, inc, dec, jnz, tgl) = (
      "cpy ([+-]?[a-z0-9]+) (\\w+)".r,
      "inc (\\w+)".r,
      "dec (\\w+)".r,
      "jnz (.+) (.+)".r,
      "tgl ([+-]?[a-z0-9]+)".r
    )

    def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    def valueOf(x: String, regs: Map[String, Int]) = if (isNumeric(x)) x.toInt else regs(x)

    def toggle(idx: Int, cms: List[String]) = {
      if (idx >= cms.size) cms
      else cms.updated(idx, cms(idx) match {
        case cpy(x, y) => s"jnz $x $y"
        case inc(x) => s"dec $x"
        case dec(x) => s"inc $x"
        case jnz(x, y) => s"cpy $x $y"
        case tgl(x) => s"inc $x"
      })
    }

    def execute(idx: Int, cms: List[String], regs: Map[String, Int]): Map[String, Int] = {
      if (idx >= commands.size) regs
      else cms(idx) match {
        case cpy(x, y) => execute(idx + 1, cms, regs + (if (isNumeric(x)) y -> x.toInt else y -> regs(x)))
        case inc(x) => execute(idx + 1, cms, regs + (x -> (regs(x) + 1)))
        case dec(x) => execute(idx + 1, cms, regs + (x -> (regs(x) - 1)))
        case jnz(x, y) => if (valueOf(x, regs) == 0) execute(idx + 1, cms, regs) else execute(idx + valueOf(y, regs), cms, regs)
        case tgl(x) =>
          if (isNumeric(x)) execute(idx + 1, toggle(idx + x.toInt, cms), regs)
          else execute(idx + 1, toggle(idx + regs(x), cms), regs)
        case _ => execute(idx + 1, cms, regs)
      }
    }

    execute(0, commands, regs.withDefaultValue(0))
  }
}
