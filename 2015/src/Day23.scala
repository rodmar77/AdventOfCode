import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Day23 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day23.txt")
    .getLines
    .toList

  def registers(data: List[String], im: Map[String, BigInt]) = {
    val (rhlf, rtpl, rinc, rjmp, rjie, rjio) = (
      "hlf (\\w+)".r,
      "tpl (\\w+)".r,
      "inc (\\w+)".r,
      "jmp ([+-]\\d+)".r,
      "jie (\\w+), ([+-]\\d+)".r,
      "jio (\\w+), ([+-]\\d+)".r)

    type RegisterMap = Map[String, BigInt]
    val operations: List[(Regex, (String, Int, RegisterMap) => (Int, RegisterMap))] = List(
      (rhlf, (op, index, m) => op match {
        case rhlf(r) => (index + 1, m + (r -> m(r) / 2))
      }),
      (rtpl, (op, index, m) => op match {
        case rtpl(r) => (index + 1, m + (r -> m(r) * 3))
      }),
      (rinc, (op, index, m) => op match {
        case rinc(r) => (index + 1, m + (r -> (m(r) + 1)))
      }),
      (rjmp, (op, index, m) => op match {
        case rjmp(offset) => (index + offset.toInt, m)
      }),
      (rjie, (op, index, m) => op match {
        case rjie(r, offset) => if (m(r) % 2 == 0) (index + offset.toInt, m) else (index + 1, m)
      }),
      (rjio, (op, index, m) => op match {
        case rjio(r, offset) => if (m(r) == 1) (index + offset.toInt, m) else (index + 1, m)
      }))

    def execute(index: Int, m: RegisterMap): RegisterMap = {
      if (index >= data.length) m
      else {
        val op = data(index)
        val k = operations
          .find(_._1
            .findFirstMatchIn(op)
            .isDefined)
          .get
          ._2(op, index, m)

        execute(k._1, k._2)
      }
    }

    execute(0, im)
  }

  println(registers(data, Map("a" -> 0, "b" -> 0))("b"))
  println(registers(data, Map("a" -> 1, "b" -> 0))("b"))
}