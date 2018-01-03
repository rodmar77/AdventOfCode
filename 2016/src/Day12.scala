import scala.io.Source

/*
  --- Day 12: Leonardo's Monorail ---

  You finally reach the top floor of this building: a garden with a slanted glass
  ceiling. Looks like there are no more stars to be had.

  While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt
  some of the files you extracted from the servers downstairs.

  According to these documents, Easter Bunny HQ isn't just this building - it's a
  collection of buildings in the nearby area. They're all connected by a local
  monorail, and there's another building not far from here! Unfortunately, being
  night, the monorail is currently not operating.

  You remotely connect to the monorail control systems and discover that the boot
  sequence expects a password. The password-checking logic (your puzzle input) is
  easy to extract, but the code it uses is strange: it's assembunny code designed
  for the new computer you just assembled. You'll have to execute the code and get
  the password.

  The assembunny code you've extracted operates on four registers (a, b, c, and d)
  that start at 0 and can hold any integer. However, it seems to make use of only
  a few instructions:

    - cpy x y copies x (either an integer or the value of a register) into register y.
    - inc x increases the value of register x by one.
    - dec x decreases the value of register x by one.
    - jnz x y jumps to an instruction y away (positive means forward; negative means
      backward), but only if x is not zero.

  The jnz instruction moves relative to itself: an offset of -1 would continue at
  the previous instruction, while an offset of 2 would skip over the next
  instruction.

  For example:

  +----------+
  | cpy 41 a |
  | inc a    |
  | inc a    |
  | dec a    |
  | jnz a 2  |
  | dec a    |
  +----------+

  The above code would set register a to 41, increase its value by 2, decrease
  its value by 1, and then skip the last dec a (because a is not zero, so the
  jnz a 2 skips it), leaving register a at 42. When you move past the last
  instruction, the program halts.

 */
object Day12 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day12.txt")
      .getLines
      .toList

    // After executing the assembunny code in your puzzle input, what value is left in
    // register a?
    println(execute(commands)("a"))

    // As you head down the fire escape to the monorail, you notice it didn't start;
    // register c needs to be initialized to the position of the ignition key.
    //
    // If you instead initialize register c to be 1, what value is now left in register a?
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
