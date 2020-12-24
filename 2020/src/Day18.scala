import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day18.txt")) {
      source =>
        val expressions = source.getLines.toList
        List(false, true).foreach(additionHasPrecedence => println(expressions.map(solve(_, additionHasPrecedence)).sum))
    }
  }

  def solve(line: String, additionHasPrecedence: Boolean): BigInt = {
    val (subExpression, addition) = (""".*\(([^)]+)\).*""".r, """(\d+) [+] (\d+)""".r)

    @tailrec
    def processString(l: String): BigInt = {
      def compute() = {
        @tailrec
        def computeTokens(ll: List[String], acc: BigInt): BigInt = ll match {
          case op :: number :: tail => op match {
            case "+" => computeTokens(tail, acc + BigInt(number))
            case "-" => computeTokens(tail, acc - BigInt(number))
            case   _ => computeTokens(tail, acc * BigInt(number))
          }
          case Nil => acc
        }

        val tokens = l.split("(\\s)")
        computeTokens(tokens.tail.toList, tokens.head.toLong)
      }

      if (additionHasPrecedence) addition.findFirstMatchIn(l) match {
        case Some(m) => processString(s"${l.substring(0, m.start)}${BigInt(m.group(1)) + BigInt(m.group(2))}${l.substring(m.end)}")
        case None => compute()
      } else {
        compute()
      }
    }

    @tailrec
    def solve(l: String): BigInt = l match {
      case subExpression(sub) => solve(l.replace(s"($sub)", processString(sub).toString))
      case _ => processString(l)
    }

    solve(line)
  }
}