import scala.util.matching.Regex._
import scala.io._

abstract class Action {
  def exec(m: Match, v: Map[String, Int]): (String, Int)
  def canExecute(m: Option[Match], v: Map[String, Int]): Boolean
  def isNumeric(v: String) = v.forall(_.isDigit)
  def toInt(n: String, v: Map[String, Int]): Int = if (isNumeric(n)) (n.toInt) else (v(n))
}

object OneOpAction { def apply(f: Int => Int) = new OneOpAction(f) }
class OneOpAction(f: Int => Int) extends Action {
  def exec(m: Match, v: Map[String, Int]): (String, Int) = (m.group(2), f(toInt(m.group(1), v)))
  def canExecute(m: Option[Match], v: Map[String, Int]) =
    ((m.isDefined) && ((v.contains(m.get.group(1))) || (isNumeric(m.get.group(1)))))
}

object TwoOpAction { def apply(f: (Int, Int) => Int) = new TwoOpAction(f) }
class TwoOpAction(f: (Int, Int) => Int) extends Action {
  def exec(m: Match, v: Map[String, Int]): (String, Int) = (
    m.group(3), f(toInt(m.group(1), v), toInt(m.group(2), v)))
  def canExecute(m: Option[Match], v: Map[String, Int]) = (
    (m.isDefined) &&
    ((v.contains(m.get.group(1))) || (isNumeric(m.get.group(1)))) &&
    ((v.contains(m.get.group(2))) || (isNumeric(m.get.group(2)))))
}

object Operation { def apply(r: String, a: Action) = new Operation(r, a) }
class Operation(val regex: String, val action: Action) {
  val pattern = regex.r
  def canExecute(l: String, v: Map[String, Int]) = action.canExecute(pattern.findFirstMatchIn(l), v)
  def execute(l: String, v: Map[String, Int]) = action.exec(pattern.findFirstMatchIn(l).get, v)
}

object Day7 extends App {
  val constantOperation = Operation("^(\\d+?) -> (.+)$", OneOpAction(_.toInt))

  val os = List[Operation](
    Operation("^(.+?) -> (.+)$", OneOpAction(_.toInt)),
    Operation("^(.+?) AND (.+?) -> (.+)$", TwoOpAction(_ & _)),
    Operation("^(.+?) OR (.+?) -> (.+)$", TwoOpAction(_ | _)),
    Operation("^(.+?) LSHIFT (\\d+?) -> (.+)$", TwoOpAction(_ << _)),
    Operation("^(.+?) RSHIFT (\\d+?) -> (.+)$", TwoOpAction(_ >> _)),
    Operation("^NOT (.+?) -> (.+)$", OneOpAction(~_)))

  val lines = Source.fromFile("/tmp/input.txt").getLines.toList
  println(getValues("a"))

  def getValues = {
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

    val (nextLines, initialMap) = getValues(lines, Map[String, Int]())
    getValues(nextLines, initialMap)._2
  }
}