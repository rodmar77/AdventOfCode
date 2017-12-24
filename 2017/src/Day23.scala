import scala.io.Source

object Day23 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2017/input_day23.txt")
      .getLines
      .toList

    println(execute(commands))
    println(compute(execute(commands)))
  }

  case class Program(
                 idx: Int = 0,
                 regs: Map[String, BigInt] = Map().withDefaultValue(0),
                 counts: Map[String, Int] = Map().withDefaultValue(0)) {

    private def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    private def increment(key: String) = counts + (key -> (counts(key) + 1))
    private def regOp(x: String, y: String, f: (BigInt, BigInt) => BigInt) = regs + (x -> f(regs(x), valueOf(y)))

    def valueOf(x: String) = if (isNumeric(x)) BigInt(x) else regs(x)
    def set(x: String, y: String) = Program(idx, regs + (x -> valueOf(y)), increment("set"))
    def sub(x: String, y: String) = Program(idx, regOp(x, y, _ - _), increment("sub"))
    def mul(x: String, y: String) = Program(idx, regOp(x, y, _ * _), increment("mul"))
    def jump(cnt: Int) = Program(idx + cnt, regs, increment("jump"))
    def jnz(x: String, y: String) = if (valueOf(x) == 0) jump(1) else jump(valueOf(y).toInt)
  }

  def execute(commands: List[String]) = {
    val (set, sub, mul, jnz) = (
      "set (.+) (.+)".r,
      "sub (.+) (.+)".r,
      "mul (.+) (.+)".r,
      "jnz (.+) (.+)".r,
    )

    def execute(p: Program): Program = {
      if (p.idx >= commands.size) p
      else commands(p.idx) match {
        case set(x, y) => execute(p.set(x, y).jump(1))
        case sub(x, y) => execute(p.sub(x, y).jump(1))
        case mul(x, y) => execute(p.mul(x, y).jump(1))
        case jnz(x, y) => execute(p.jnz(x, y))
      }
    }

    execute(Program())
  }

  def compute(program: Program) = {
    def prime(n: BigInt): Boolean  = {
      if (n % 2 == 0) false
      else !(3 to Math.sqrt(n.toLong).toInt by 2).exists(n.toLong % _ == 0)
    }

    val b = program.regs("b") * 100 + 100000
    val c = b + 17000

    (b to c by 17).count(!prime(_))
  }
}
