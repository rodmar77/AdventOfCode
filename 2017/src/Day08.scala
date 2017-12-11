import scala.io.Source

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
    println(m.values.map(_.last).max)
    println(m.values.map(_.max).max)
  }
}
