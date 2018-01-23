import scala.io.Source

/*

  --- Day 25: Clock Signal ---

  You open the door and find yourself on the roof. The city sprawls away from
  you for miles and miles.

  There's not much time now - it's already Christmas, but you're nowhere near
  the North Pole, much too far to deliver these stars to the sleigh in time.

  However, maybe the huge antenna up here can offer a solution. After all, the
  sleigh doesn't need the stars, exactly; it needs the timing data they provide,
  and you happen to have a massive signal generator right here.

  You connect the stars you have to your prototype computer, connect that to the
  antenna, and begin the transmission.

  Nothing happens.

  You call the service number printed on the side of the antenna and quickly
  explain the situation. "I'm not sure what kind of equipment you have connected
  over there," he says, "but you need a clock signal." You try to explain that
  this is a signal for a clock.

  "No, no, a clock signal - timing information so the antenna computer knows how
  to read the data you're sending it. An endless, alternating pattern of 0, 1,
  0, 1, 0, 1, 0, 1, 0, 1...." He trails off.

  You ask if the antenna can handle a clock signal at the frequency you would need
  to use for the data from the stars. "There's no way it can! The only antenna we've
  installed capable of that is on top of a top-secret Easter Bunny installation,
  and you're definitely not-" You hang up the phone.

  You've extracted the antenna's clock signal generation assembunny code (your
  puzzle input); it looks mostly compatible with code you worked on just recently.

  This antenna code, being a signal generator, uses one extra instruction:

  out x transmits x (either an integer or the value of a register) as the next
  value for the clock signal.

  The code takes a value (via register a) that describes the signal to generate,
  but you're not sure how it's used. You'll have to find the input to produce
  the right signal through experimentation.

 */
object Day25 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day25.txt")
      .getLines
      .toList

    // What is the lowest positive integer that can be used to initialize register
    // a and cause the code to output a clock signal of 0, 1, 0, 1... repeating
    // forever?
    val ll = (0 to 100).map(_ % 2).toList
    Stream.from(1).find(a => verifyExecution(commands, Map("a" -> a), ll)) match {
      case Some(a) => println(a)
    }
  }

  def verifyExecution(commands: List[String], regs: Map[String, Int], input: List[Int]) = {
    val (cpy, inc, dec, jnz, out) = (
      "cpy ([+-]?[a-z0-9]+) (\\w+)".r,
      "inc (\\w+)".r,
      "dec (\\w+)".r,
      "jnz (\\w+) ([+-]?\\d+)".r,
      "out (.+)".r
    )

    def valueOf(key: String, regs: Map[String, Int]) = if (isNumeric(key)) key.toInt else regs(key)
    def isNumeric(str: String) = str.matches("([+-]?[0-9]+)")
    def isZero(x: String, regs: Map[String, Int]) = {
      if (isNumeric(x)) x.toInt == 0
      else regs(x) == 0
    }

    def execute(idx: Int, regs: Map[String, Int], input: List[Int]): Boolean = {
      if (input.isEmpty) true
      else if (idx >= commands.size) false
      else commands(idx) match {
        case cpy(x, y) => execute(idx + 1, regs + (y -> valueOf(x, regs)), input)
        case inc(x) => execute(idx + 1, regs + (x -> (regs(x) + 1)), input)
        case dec(x) => execute(idx + 1, regs + (x -> (regs(x) - 1)), input)
        case jnz(x, y) => if (isZero(x, regs)) execute(idx + 1, regs, input) else execute(idx + y.toInt, regs, input)
        case out(x) => if (valueOf(x, regs) == input.head) execute(idx + 1, regs, input.tail) else false
      }
    }

    execute(0, regs.withDefaultValue(0), input)
  }
}
