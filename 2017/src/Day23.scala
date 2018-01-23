import scala.io.Source

/*

  --- Day 23: Coprocessor Conflagration ---

  You decide to head directly to the CPU and fix the printer from there. As you
  get close, you find an experimental coprocessor doing so much work that the
  local programs are afraid it will halt and catch fire. This would cause serious
  issues for the rest of the computer, so you head in and see what you can do.

  The code it's running seems to be a variant of the kind you saw recently on
  that tablet. The general functionality seems very similar, but some of the
  instructions are different:

    - set X Y sets register X to the value of Y.
    - sub X Y decreases register X by the value of Y.
    - mul X Y sets register X to the result of multiplying the value contained
      in register X by the value of Y.
    - jnz X Y jumps with an offset of the value of Y, but only if the value of
      X is not zero. (An offset of 2 skips the next instruction, an offset of -1
      jumps to the previous instruction, and so on.)

      Only the instructions listed above are used. The eight registers here, named
      a through h, all start at 0.

  The coprocessor is currently set to some kind of debug mode, which allows for
  testing, but prevents it from doing any meaningful work.

 */
object Day23 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2017/input_day23.txt")
      .getLines
      .toList

    // If you run the program (your puzzle input), how many times is the mul
    // instruction invoked?
    println(execute(commands).counts("mul"))

    /*
    Now, it's time to fix the problem.

    The debug mode switch is wired directly to register a. You flip the switch,
    which makes register a now start at 1 when the program is executed.

    Immediately, the coprocessor begins to overheat. Whoever wrote this program
    obviously didn't choose a very efficient implementation. You'll need to
    optimize the program if it has any hope of completing before Santa needs that
    printer working.

    The coprocessor's ultimate goal is to determine the final value left in
    register h once the program completes. Technically, if it had that... it
    wouldn't even need to run the program.

    After setting register a to 1, if the program were to run to completion,
    what value would be left in register h?
     */
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
