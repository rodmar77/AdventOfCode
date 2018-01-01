import scala.io.Source
import scala.util.parsing.json.JSON

/*

  --- Day 12: JSAbacusFramework.io ---

  Santa's Accounting-Elves need help balancing the books after a recent order.
  Unfortunately, their accounting software uses a peculiar storage format.
  That's where you come in.

  They have a JSON document which contains a variety of things: arrays ([1,2,3]),
  objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find
  all of the numbers throughout the document and add them together.

  For example:

  * [1,2,3] and {"a":2,"b":4} both have a sum of 6.
  * [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
  * {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
  * [] and {} both have a sum of 0.

  You will not encounter any strings containing numbers.

 */
object Day12 extends App {

  val text = Source
    .fromFile("inputs/2015/input_day12.txt")
    .getLines
    .mkString

  // What is the sum of all numbers in the document?
  println(sumNumbers(text))

  /*

  Uh oh - the Accounting-Elves have realized that they double-counted
  everything red.

  Ignore any object (and all of its children) which has any property with the
  value "red". Do this only for objects ({...}), not arrays ([...]).

  * [1,2,3] still has a sum of 6.
  * [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.

  * {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure
    is ignored.

  * [1,"red",5] has a sum of 6, because "red" in an array has no effect.

  */
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
    def hasRedProperty(m: Map[_, _]) = {
      def isRedProperty(n: Any): Boolean = n match {
        case s: String => s.equals("red")
        case _ => false
      }

      m.values.exists(isRedProperty)
    }

    def sumNumbersWithoutRed(n: Any): Int = n match {
      case m: Map[_, _] => if (hasRedProperty(m)) 0 else m.values.map(sumNumbersWithoutRed).sum
      case l: List[_] => l.map(sumNumbersWithoutRed).sum
      case n: Number => n.intValue
      case _ => 0
    }

    sumNumbersWithoutRed(JSON.parseFull(text).get)
  }

}
