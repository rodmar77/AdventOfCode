import scala.io._
import scala.util.matching.Regex._

/*

  --- Day 7: Some Assembly Required ---

  This year, Santa brought little Bobby Tables a set of wires and bitwise logic
  gates! Unfortunately, little Bobby is a little under the recommended age range,
  and he needs help assembling the circuit.

  Each wire has an identifier (some lowercase letters) and can carry a 16-bit
  signal (a number from 0 to 65535). A signal is provided to each wire by a gate,
  another wire, or some specific value. Each wire can only get a signal from one
  source, but can provide its signal to multiple destinations. A gate provides no
  signal until all of its inputs have a signal.

  The included instructions booklet describes how to connect the parts together:
  x AND y -> z means to connect wires x and y to an AND gate, and then connect its
  output to wire z.

  For example:

  123 -> x means that the signal 123 is provided to wire x.
  x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
  p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and thenprovided to wire q.
  NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

  Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some
  reason, you'd like to emulate the circuit instead, almost all programming languages
  (for example, C, JavaScript, or Python) provide operators for these gates.

  For example, here is a simple circuit:

  +-----------------+
  | 123 -> x        |
  | 456 -> y        |
  | x AND y -> d    |
  | x OR y -> e     |
  | x LSHIFT 2 -> f |
  | y RSHIFT 2 -> g |
  | NOT x -> h      |
  | NOT y -> i      |
  +-----------------+

  After it is run, these are the signals on the wires:

  +-----------+
  | d: 72     |
  | e: 507    |
  | f: 492    |
  | g: 114    |
  | h: 65412  |
  | i: 65079  |
  | x: 123    |
  | y: 456    |
  +-----------+

 */
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
      m.isDefined && (v.contains(m.get.group(1)) || isNumeric(m.get.group(1)))
  }

  case class TwoOpWire(f: (Int, Int) => Int) extends Wire {
    def exec(m: Match, v: Map[String, Int]): (String, Int) = (
      m.group(3), f(toInt(m.group(1), v), toInt(m.group(2), v)))
    def canExecute(m: Option[Match], v: Map[String, Int]) =
      m.isDefined &&
        (v.contains(m.get.group(1)) || isNumeric(m.get.group(1))) &&
        (v.contains(m.get.group(2)) || isNumeric(m.get.group(2)))
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

  // In little Bobby's kit's instructions booklet (provided as your puzzle input),
  // what signal is ultimately provided to wire a?
  val a = getValues(lines)("a")
  println(a)

  // Now, take the signal you got on wire a, override wire b to that signal, and
  // reset the other wires (including wire a). What new signal is ultimately
  // provided to wire a?
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