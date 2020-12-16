import scala.io.Source
import scala.util.Using

object Day08 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day08.txt")) {
      source =>
        val regex = "([a-z]{3}) ([+-][0-9]+)".r
        val ops = source.getLines.map {
          case regex(op, value) => (op, value.toInt)
        }.toList

        println(terminates(ops) match {
          case (_, acc) => acc
        })

        val x = ops.zipWithIndex.map {
          case ((op, value), idx) => op match {
            case "nop" => terminates(ops.updated(idx, ("jmp", value)))
            case "jmp" => terminates(ops.updated(idx, ("nop", value)))
            case _ => (false, -1)
          }
        }.find  {
          case (terminates, _) => terminates
        } match {
          case Some((_, acc)) => acc
        }

        println(x)
    }
  }

  def terminates(ops: List[(String, Int)]) = {
    def terminates(idx: Int, acc: Int, visited: Set[Int]): (Boolean, Int) = {
      if (visited.contains(idx)) (false, acc)
      else if (idx < 0 || idx >= ops.size) (true, acc)
      else ops(idx) match {
        case (op, value) => op match {
          case "nop" => terminates(idx + 1, acc, visited + idx)
          case "acc" => terminates(idx + 1, acc + value, visited + idx)
          case _ => terminates(idx + value, acc, visited + idx)
        }
      }
    }

    terminates(0, 0, Set())
  }

}

