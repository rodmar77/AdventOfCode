import scala.io.Source
import scala.util.parsing.json.JSON

object Day12 extends App {

  val text = Source
    .fromFile("inputs/2015/input_day12.txt")
    .getLines
    .mkString

  println(sumNumbers(text))
  println(secondPart(text))

  def sumNumbers(text: String) =
    """(-?\d+)"""
      .r
      .findAllMatchIn(
        text
          .replaceAll( """"[^"]+"""", ""))
      .map(_.group(0).toInt)
      .sum

  def secondPart(text: String) = {
    def hasRedProperty(m: Map[Any, Any]) = {
      def isRedProperty(n: Any): Boolean = n match {
        case s: String => s.equals("red")
        case _ => false
      }

      m.values.exists(isRedProperty)
    }
    def sumNumbersWithoutRed(n: Any): Int = n match {
      case m: Map[Any, Any] => if (hasRedProperty(m)) 0 else m.values.map(sumNumbersWithoutRed).sum
      case l: List[Any] => l.map(sumNumbersWithoutRed).sum
      case n: Number => n.intValue
      case _ => 0
    }

    sumNumbersWithoutRed(JSON.parseFull(text).get)
  }

}
