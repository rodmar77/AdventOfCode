import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Day23 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day23.txt")
    .getLines
    .toList

  def registers(im: Map[String, Int]) = {
    val instructions = List[(Regex, (Map[String, Int], Int, Seq[String]) => (Map[String, Int], Int))](
      ( """hlf (.+)""".r, (m, c, p) => (m + (p.head -> m(p.head) / 2), c + 1)),
      ( """tpl (.+)""".r, (m, c, p) => (m + (p.head -> m(p.head) * 3), c + 1)),
      ( """inc (.+)""".r, (m, c, p) => (m + (p.head -> (m(p.head) + 1)), c + 1)),
      ( """jmp (.+)""".r, (m, c, p) => (m, c + p.head.toInt)),
      ( """jie (.+?), (.+)""".r, (m, c, p) => (m, c + (if (m(p.head) % 2 == 0) p.last.toInt else 1))),
      ( """jio (.+?), (.+)""".r, (m, c, p) => (m, c + (if (m(p.head) == 1) p.last.toInt else 1))))

    def nextInstruction(idx: Int) = instructions.find(p => p._1.findFirstMatchIn(data(idx)).nonEmpty).get
    def toList(groups: Match) = (1 to groups.groupCount).map(groups.group)

    def nextParams(idx: Int) = {
      val instruction = nextInstruction(idx)
      val groups = instruction._1.findFirstMatchIn(data(idx)).get
      (instruction._2, toList(groups))
    }

    def nextData(vals: Map[String, Int], idx: Int) = {
      val (instruction, params) = nextParams(idx)
      instruction(vals, idx, params)
    }

    def getMap(vals: Map[String, Int], idx: Int): Map[String, Int] = {
      if (idx == data.size) vals
      else {
        val (nextMap, nextIdx) = nextData(vals, idx)
        getMap(nextMap, nextIdx)
      }
    }

    getMap(im, 0)
  }

  println(registers(Map("a" -> 0, "b" -> 0))("b"))
  println(registers(Map("a" -> 1, "b" -> 0))("b"))
}