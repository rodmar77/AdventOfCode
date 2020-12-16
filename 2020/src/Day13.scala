import scala.io.Source
import scala.util.Using

object Day13 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day13.txt")) {
      source =>
        val text = source.getLines.toList
        val (n, k) = (text.head.toInt, text.last.split(",").toList)

        println(k
          .filterNot(_.equals("x"))
          .map(_.toInt)
          .map(x => {
            (x, x - n%x)
          })
          .minBy {
            case (_, diff) => diff
          } match {
            case (id, diff) => id * diff
          })
    }
  }
}
