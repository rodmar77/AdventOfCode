import scala.io.Source

object Day18 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2017/input_day18.txt")
      .getLines
      .toList

    println(execute(commands))
    println(executeWithTwoPrograms(commands).sent.size)
  }

  case class Program(
                idx: Int = 0,
                regs: Map[String, BigInt] = Map().withDefaultValue(0),
                q: List[BigInt] = List(),
                sent: List[BigInt] = List(),
                isRunning: Boolean = true) {

    private def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    
    def valueOf(x: String) = if (isNumeric(x)) BigInt(x) else regs(x)
    def send(x: String) = Program(idx, regs, q, sent :+ valueOf(x), isRunning)
    def addToQueue(y: BigInt) = Program(idx, regs, q :+ y, sent, isRunning)
    def set(x: String, y: String) = Program(idx, regs + (x -> valueOf(y)), q, sent, isRunning)
    def add(x: String, y: String) = Program(idx, regs + (x -> (regs(x) + valueOf(y))), q, sent, isRunning)
    def mul(x: String, y: String) = Program(idx, regs + (x -> (regs(x) * valueOf(y))), q, sent, isRunning)
    def mod(x: String, y: String) = Program(idx, regs + (x -> (regs(x) % valueOf(y))), q, sent, isRunning)
    def jump(cnt: Int) = Program(idx + cnt, regs, q, sent, isRunning)
    def jgz(x: String, y: String) = if (valueOf(x) > 0) jump(valueOf(y).toInt) else jump(1)
    def canConsume = q.nonEmpty
    def consume(x: String) = Program(idx, regs + (x -> q.head), q.tail, sent)
    def pause() = Program(idx, regs, q, sent, isRunning = false)
  }

  def execute(commands: List[String]) = {
    val (snd, set, add, mul, mod, rcv, jgz) = (
      "snd (.+)".r,
      "set (.+) (.+)".r,
      "add (.+) (.+)".r,
      "mul (.+) (.+)".r,
      "mod (.+) (.+)".r,
      "rcv (.+)".r,
      "jgz (.+) (.+)".r,
    )

    def execute(p: Program): BigInt = {
      if (p.idx >= commands.size) -1
      else commands(p.idx) match {
        case snd(x) => execute(p.send(x).jump(1))
        case set(x, y) => execute(p.set(x, y).jump(1))
        case add(x, y) => execute(p.add(x, y).jump(1))
        case mul(x, y) => execute(p.mul(x, y).jump(1))
        case mod(x, y) => execute(p.mod(x, y).jump(1))
        case rcv(x) => if (p.sent.isEmpty || p.sent.last == 0 || p.valueOf(x) == 0)
          execute(p.jump(1))
        else
          p.sent.last
        case jgz(x, y) => execute(p.jgz(x, y))
      }
    }

    execute(Program())
  }

  def executeWithTwoPrograms(commands: List[String]) = {
    val (snd, set, add, mul, mod, rcv, jgz) = (
      "snd (.+)".r,
      "set (.+) (.+)".r,
      "add (.+) (.+)".r,
      "mul (.+) (.+)".r,
      "mod (.+) (.+)".r,
      "rcv (.+)".r,
      "jgz (.+) (.+)".r,
    )

    def executeFirstProgram(a: Program, b: Program) = commands(a.idx) match {
      case snd(x) => (a.send(x).jump(1), b.addToQueue(a.valueOf(x)))
      case set(x, y) => (a.set(x, y).jump(1), b)
      case add(x, y) => (a.add(x, y).jump(1), b)
      case mul(x, y) => (a.mul(x, y).jump(1), b)
      case mod(x, y) => (a.mod(x, y).jump(1), b)
      case rcv(x) => if (a.canConsume) (a.consume(x).jump(1), b) else (a.pause(), b)
      case jgz(x, y) => (a.jgz(x, y), b)
    }

    def executePrograms(p: Program, q: Program): Program = {
      if (p.idx >= commands.size || q.idx >= commands.size) q
      else if (p.isRunning || p.canConsume)
        executeFirstProgram(p, q) match {
          case (np, nq) => executePrograms(np, nq)
        }
      else if (q.isRunning || q.canConsume)
        executeFirstProgram(q, p) match {
          case (nq, np) => executePrograms(np, nq)
        }
      else q
    }

    executePrograms(
      Program(0, Map("p" -> BigInt(0)).withDefaultValue(0)),
      Program(0, Map("p" -> BigInt(1)).withDefaultValue(0)))
  }
}
