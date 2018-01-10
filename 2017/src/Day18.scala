import scala.io.Source

/*

  --- Day 18: Duet ---

  You discover a tablet containing some strange assembly code labeled simply
  "Duet". Rather than bother the sound card with it, you decide to run the code
  yourself. Unfortunately, you don't see any documentation, so you're left to
  figure out what the instructions mean on your own.

  It seems like the assembly is meant to operate on a set of registers that are
  each named with a single letter and that can each hold a single integer. You
  suppose each register should start with a value of 0.

  There aren't that many instructions, so it shouldn't be hard to figure out what
  they do. Here's what you determine:

    - [snd X] plays a sound with a frequency equal to the value of X.

    - [set X Y] sets register X to the value of Y.

    - [add X Y] increases register X by the value of Y.

    - [mul X Y] sets register X to the result of multiplying the value contained
      in register X by the value of Y.

    - [mod X Y] sets register X to the remainder of dividing the value contained
      in register X by the value of Y (that is, it sets X to the result of X
      modulo Y).

    - [rcv X] recovers the frequency of the last sound played, but only when the
      value of X is not zero. (If it is zero, the command does nothing.)

    - [jgz X Y] jumps with an offset of the value of Y, but only if the value of
      X is greater than zero. (An offset of 2 skips the next instruction, an
      offset of -1 jumps to the previous instruction, and so on.)

  Many of the instructions can take either a register (a single letter) or a
  number. The value of a register is the integer it contains; the value of a
  number is that number.

  After each jump instruction, the program continues with the instruction to which
  the jump jumped. After any other instruction, the program continues with the
  next instruction. Continuing (or jumping) off either end of the program terminates
  it.

  For example:

  +----------+
  | set a 1  |
  | add a 2  |
  | mul a a  |
  | mod a 5  |
  | snd a    |
  | set a 0  |
  | rcv a    |
  | jgz a -1 |
  | set a 1  |
  | jgz a -2 |
  +----------+

    - The first four instructions set a to 1, add 2 to it, square it, and then
      set it to itself modulo 5, resulting in a value of 4.

    - Then, a sound with frequency 4 (the value of a) is played.

    - After that, a is set to 0, causing the subsequent rcv and jgz instructions
      to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).

    - Finally, a is set to 1, causing the next jgz instruction to activate, jumping
      back two instructions to another jump, which jumps again to the rcv, which
      ultimately triggers the recover operation.

  At the time the recover operation is executed, the frequency of the last sound
  played is 4.

 */
object Day18 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2017/input_day18.txt")
      .getLines
      .toList

    // What is the value of the recovered frequency (the value of the most recently
    // played sound) the first time a rcv instruction is executed with a non-zero
    // value?
    println(execute(commands))

    /*
    As you congratulate yourself for a job well done, you notice that the
    documentation has been on the back of the tablet this entire time. While you
    actually got most of the instructions correct, there are a few key differences.
    This assembly code isn't about sound at all - it's meant to be run twice at
    the same time.

    Each running copy of the program has its own set of registers and follows the
    code independently - in fact, the programs don't even necessarily run at the
    same speed. To coordinate, they use the send (snd) and receive (rcv) instructions:

      - [snd X] sends the value of X to the other program. These values wait in a
        queue until that program is ready to receive them. Each program has its
        own message queue, so a program can never receive a message it sent.

      - [rcv X] receives the next value and stores it in register X. If no values
        are in the queue, the program waits for a value to be sent to it. Programs
        do not continue to the next instruction until they have received a value.
        Values are received in the order they are sent.

    Each program also has its own program ID (one 0 and the other 1); the register
    p should begin with this value.

    For example:

    +-------+
    | snd 1 |
    | snd 2 |
    | snd p |
    | rcv a |
    | rcv b |
    | rcv c |
    | rcv d |
    +-------+

    Both programs begin by sending three values to the other. Program 0 sends 1,
    2, 0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1)
    and stores it in a, receives another value (both 2) and stores it in b, and
    then each receives the program ID of the other program (program 0 receives 1;
    program 1 receives 0) and stores it in c. Each program now sees a different
    value in its own copy of register c.

    Finally, both programs try to rcv a fourth time, but no data is waiting for
    either of them, and they reach a deadlock. When this happens, both programs
    terminate.

    It should be noted that it would be equally valid for the programs to run at
    different speeds; for example, program 0 might have sent all three values and
    then stopped at the first rcv before program 1 executed even its first
    instruction.

    Once both of your programs have terminated (regardless of what caused them to
    do so), how many times did program 1 send a value?
    */
    println(executeWithTwoPrograms(commands).sent.size)
  }

  val (snd, set, add, mul, mod, rcv, jgz) = (
    "snd (.+)".r,
    "set (.+) (.+)".r,
    "add (.+) (.+)".r,
    "mul (.+) (.+)".r,
    "mod (.+) (.+)".r,
    "rcv (.+)".r,
    "jgz (.+) (.+)".r,
  )

  case class Program(
                idx: Int = 0,
                regs: Map[String, BigInt] = Map().withDefaultValue(0),
                q: List[BigInt] = List(),
                sent: List[BigInt] = List(),
                isRunning: Boolean = true) {

    private def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    private def regOp(x: String, y: String, f: (BigInt, BigInt) => BigInt) = regs + (x -> f(regs(x), valueOf(y)))

    def valueOf(x: String) = if (isNumeric(x)) BigInt(x) else regs(x)
    def send(x: String) = Program(idx, regs, q, sent :+ valueOf(x), isRunning)
    def addToQueue(y: BigInt) = Program(idx, regs, q :+ y, sent, isRunning)
    def set(x: String, y: String) = Program(idx, regs + (x -> valueOf(y)), q, sent, isRunning)
    def add(x: String, y: String) = Program(idx, regOp(x, y, _ + _), q, sent, isRunning)
    def mul(x: String, y: String) = Program(idx, regOp(x, y, _ * _), q, sent, isRunning)
    def mod(x: String, y: String) = Program(idx, regOp(x, y, _ % _), q, sent, isRunning)
    def jump(cnt: Int) = Program(idx + cnt, regs, q, sent, isRunning)
    def jgz(x: String, y: String) = if (valueOf(x) > 0) jump(valueOf(y).toInt) else jump(1)
    def canConsume = q.nonEmpty
    def consume(x: String) = Program(idx, regs + (x -> q.head), q.tail, sent)
    def pause() = Program(idx, regs, q, sent, isRunning = false)
  }

  def execute(commands: List[String]) = {
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
