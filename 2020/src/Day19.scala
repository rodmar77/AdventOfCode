import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day19 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day19.txt")) {
      src => {
        val lines = src.getLines.toList
        val ruleLines = lines.takeWhile(_.nonEmpty)
        val rules = toRules(ruleLines)

        val (texts, regex) = (lines.drop(rules.size + 1), simplify(rules))
        println(texts.count(_.matches(regex)))

        val newRegex = simplify(toRules(ruleLines.map {
          case s"8: $tail" => "8: " + (1 until 6).map("42 " * _).mkString("| ").trim
          case s"11: $tail" => "11: " + (1 until 6).map(k => ("42 " * k) + ("31 " * k)).mkString("| ").trim
          case s"$rest" => rest
        }))
        println(texts.count(_.matches(newRegex)))
      }
    }
  }

  def toRules(ll: List[String]): Map[Int, String] = ll
    .map(_.split(":").toList match {
      case a :: b :: Nil => a.toInt -> b
        .split("\\|")
        .map(t => s"(${t.trim.replaceAll("(\\d+)", "($1)")})")
        .mkString("|")
        .replaceAll("\\(\"([ab])\"\\)", "($1)")
    }).toMap

  def simplify(rules: Map[Int, String]): String = {
    def replace(value: String, toReplace: Map[Int, String]): String = {
      @tailrec
      def exec(v: String, isNumber: Boolean, n: Int, acc: String): String = {
        if (v.isEmpty) acc
        else if (isNumber) {
          if (!Character.isDigit(v.head)) {
            if (toReplace.contains(n)) exec(v.tail, isNumber = false, 0, acc + toReplace(n) + v.head)
            else exec(v.tail, !isNumber, 0, acc + n + v.head)
          } else exec(v.tail, isNumber, n*10 + (v.head - '0'), acc)
        } else if (Character.isDigit(v.head)) exec(v.tail, isNumber = true, (v.head - '0'), acc)
        else exec(v.tail, isNumber, n, acc + v.head)
      }

      exec(value, isNumber = false, 0, "")
    }

    @tailrec
    def simplify(cr: Map[Int, String]): String = {
      if (cr.size == 1) cr.head match {
        case (_, value) => value.replaceAll(" ", "")
      } else {
        val toReplace = cr.filter {
          case (_, value) => value.matches("^[ |(ab)]+$")
        }

        simplify(cr.view.filterKeys(!toReplace.contains(_)).map {
          case (key, value) => (key, replace(value, toReplace))
        }.toMap)
      }
    }

    simplify(rules)
  }
}
