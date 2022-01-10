import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day16.txt")) {
      source =>
        val digits = source.mkString
        println(applyPhases(digits, 100).take(8))
    }
  }

  @tailrec
  def applyPhases(digits: String, phaseCount: Int): String = {
    def applyMultiplier(v: Int) = {
      def getMultipliers: List[Int] = {
        @tailrec
        def getMultipliers(acc: List[Int], valueType: Int): List[Int] = {
          if (acc.size > digits.length) acc.take(digits.length)
          else getMultipliers(acc ++ List.fill(v + 1)(valueType) ++ List.fill(v + 1)(0), valueType * -1)
        }

        getMultipliers(List.fill(v)(0), 1)
      }

      digits.zip(getMultipliers).map {
        case (a, b) => ((a - '0') * b) % 10
      }.sum
    }

    if (phaseCount == 0) digits
    else applyPhases((0 until digits.length).map(k => (applyMultiplier(k) % 10).abs).mkString, phaseCount - 1)
  }


}
