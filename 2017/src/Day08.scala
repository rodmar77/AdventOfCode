import scala.io.Source

/*
  --- Day 8: I Heard You Like Registers ---

  You receive a signal directly from the CPU. Because of your recent assistance
  with jump instructions, it would like you to compute the result of a series
  of unusual register instructions.

  Each instruction consists of several parts: the register to modify, whether
  to increase or decrease that register's value, the amount by which to increase
  or decrease it, and a condition. If the condition fails, skip the instruction
  without modifying the register. The registers all start at 0. The instructions
  look like this:

  +----------------------+
  | b inc 5 if a > 1     |
  | a inc 1 if b < 5     |
  | c dec -10 if a >= 1  |
  | c inc -20 if c == 10 |
  +----------------------+

  These instructions would be processed as follows:

    - Because a starts at 0, it is not greater than 1, and so b is not modified.
    - a is increased by 1 (to 1) because b is less than 5 (it is 0).
    - c is decreased by -10 (to 10) because a is now greater than or equal to 1
      (it is 1).
    - c is increased by -20 (to -10) because c is equal to 10.

  After this process, the largest value in any register is 1.

  You might also encounter <= (less than or equal to) or != (not equal to).
  However, the CPU doesn't have the bandwidth to tell you what all the registers
  are named, and leaves that to you to determine.

 */
object Day08 {

  case class Operation(operation: String) {
    val data = "(\\w+) (inc|dec) (-?\\d+) if (\\w+) (.+) (-?\\d+)".r

    def applyOperation(m: Map[String, List[Int]]): Map[String, List[Int]] = {
      def newValue(reg_1: String, op: String, amount: String): List[Int] = {
        val oldValue = m(reg_1)
        if (op.equals("inc")) oldValue :+ (oldValue.last + amount.toInt)
        else oldValue :+ (oldValue.last - amount.toInt)
      }

      def doApply(reg: String, cmp: String, cmpValue: Int): Boolean = {
        val actualValue: Int = m(reg).last
        if (cmp.equals("==")) actualValue == cmpValue
        else if (cmp.equals("<=")) actualValue <= cmpValue
        else if (cmp.equals(">=")) actualValue >= cmpValue
        else if (cmp.equals(">"))  actualValue > cmpValue
        else if (cmp.equals("<"))  actualValue < cmpValue
        else if (cmp.equals("!="))  actualValue != cmpValue
        else throw new IllegalArgumentException(s"Unknown comparison: $reg $cmp $cmpValue")
      }

      val data(reg_1, op, amount, reg_2, cmp, cmpValue) = operation

      if (doApply(reg_2, cmp, cmpValue.toInt)) m + (reg_1 -> newValue(reg_1, op, amount))
      else m
    }
  }

  def main(args: Array[String]): Unit = {
    val operations = Source
      .fromFile("inputs/2017/input_day08.txt")
      .getLines
      .map(Operation)

    val m = operations.foldLeft(Map[String, List[Int]]().withDefaultValue(List(0)))((a, b) => b.applyOperation(a))

    // What is the largest value in any register after completing the instructions
    // in your puzzle input?
    println(m.values.map(_.last).max)

    // To be safe, the CPU also needs to know the highest value held in any register
    // during this process so that it can decide how much memory to allocate to
    // these operations. For example, in the above instructions, the highest value
    // ever held was 10 (in register c after the third instruction was evaluated).
    println(m.values.map(_.max).max)
  }
}
