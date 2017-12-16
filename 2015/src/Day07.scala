import scala.io._
import scala.util.matching.Regex._

object Day07 extends App {

  sealed trait Wire {
    def exec(m: Match, v: Map[String, Int]): (String, Int)
    def canExecute(m: Option[Match], v: Map[String, Int]): Boolean
    def isNumeric(v: String) = v.forall(_.isDigit)
    def toInt(n: String, v: Map[String, Int]): Int = if (isNumeric(n)) n.toInt else v(n)
  }

  case class OneOpWire(f: Int => Int) extends Wire {
    def exec(m: Match, v: Map[String, Int]): (String, Int) = (m.group(2), f(toInt(m.group(1), v)))
    def canExecute(m: Option[Match], v: Map[String, Int]) =
      ((m.isDefined) && ((v.contains(m.get.group(1))) || (isNumeric(m.get.group(1)))))
  }

  case class TwoOpWire(f: (Int, Int) => Int) extends Wire {
    def exec(m: Match, v: Map[String, Int]): (String, Int) = (
      m.group(3), f(toInt(m.group(1), v), toInt(m.group(2), v)))
    def canExecute(m: Option[Match], v: Map[String, Int]) = (
      (m.isDefined) &&
        ((v.contains(m.get.group(1))) || (isNumeric(m.get.group(1)))) &&
        ((v.contains(m.get.group(2))) || (isNumeric(m.get.group(2)))))
  }

  case class Operation(regex: String, wire: Wire) {
    val pattern = regex.r
    def canExecute(l: String, v: Map[String, Int]) = wire.canExecute(pattern.findFirstMatchIn(l), v)
    def execute(l: String, v: Map[String, Int]) = wire.exec(pattern.findFirstMatchIn(l).get, v)
  }

  val constantOperation = Operation("^(\\d+?) -> (.+)$", OneOpWire(_.toInt))

  val os = List(
    Operation("^(.+?) -> (.+)$", OneOpWire(_.toInt)),
    Operation("^(.+?) AND (.+?) -> (.+)$", TwoOpWire(_ & _)),
    Operation("^(.+?) OR (.+?) -> (.+)$", TwoOpWire(_ | _)),
    Operation("^(.+?) LSHIFT (\\d+?) -> (.+)$", TwoOpWire(_ << _)),
    Operation("^(.+?) RSHIFT (\\d+?) -> (.+)$", TwoOpWire(_ >> _)),
    Operation("^NOT (.+?) -> (.+)$", OneOpWire(~_)))

  val lines = Source.fromFile("inputs/2015/input_day07.txt").getLines.toList

  val a = getValues(lines)("a")
  println(a)
  println(getValues(lines.map(s => if (s.endsWith(" -> b")) s"$a -> b" else s))("a"))

  def getValues(lines: List[String]) = {
    def getValues(ls: Seq[String], v: Map[String, Int]): (Seq[String], Map[String, Int]) = {
      if (ls.isEmpty) (ls, v)
      else {
        val pairs = ls
          .map(l => (l, os.find(_.canExecute(l, v))))
          .filter(_._2.isDefined)
          .map { case (l, o) => (l, o.get) }

        if (pairs.isEmpty) (ls, v)
        else getValues(
          ls.diff(pairs.map(_._1)),
          v ++ pairs.map { case (l, o) => o.execute(l, v) })
      }
    }

    val (nextLines, initialMap) = getValues(lines, Map())
    getValues(nextLines, initialMap)._2
  }
}