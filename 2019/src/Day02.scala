import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day02.txt")) {
      source =>
        val numbers = source.mkString.split(",").map(_.toInt).toList
        println(process(numbers, 12, 2))
        println(produces(numbers, 19690720))
    }
  }

  def produces(value: List[Int], expected: Int): Int = {
    (0 to 99).flatMap(noun => (0 to 99).map((noun, _))).find {
      case (noun, verb) => process(value, noun, verb) == expected
    } match {
      case Some((noun, verb)) => noun*100 + verb
      case None => -1
    }
  }

  def process(value: List[Int], noun: Int, verb: Int): Int = {
    @tailrec
    def process(ll: List[Int], idx: Int): Int = {
      if (ll(idx) == 99) ll.head
      else ll(idx) match {
        case 1 => process(ll.updated(ll(idx + 3), ll(ll(idx + 1)) + ll(ll(idx + 2))), idx + 4)
        case 2 => process(ll.updated(ll(idx + 3), ll(ll(idx + 1)) * ll(ll(idx + 2))), idx + 4)
        case _ => -1
      }
    }

    process(value.updated(1, noun).updated(2, verb), 0)
  }

}
