import Day10.countCombinations

import scala.io.Source
import scala.util.Using

object Day13 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day13.txt")) {
      source =>
        val text = source.getLines.toList
        val (n, k) = (text.head.toInt, text.last.split(",").toList)

        println(k
          .filterNot(_.equals("x"))
          .map(_.toInt)
          .map(x => {
            (x, x - n%x)
          })
          .minBy {
            case (_, diff) => diff
          } match {
            case (id, diff) => id * diff
          })

        val ignored = "x"
        val x = k.zipWithIndex.map {
          case (`ignored`, _) => (-1, -1)
          case (value, index) => (value.toInt, ((value.toInt - index) % value.toInt).abs)
        } filterNot {
          case (v, _) => v == -1
        }

        var (r, i) = (BigInt(0), BigInt(1))
        x.foreach {
          case (v, u) => {
            while (r % v != u) {
              r += i
            }

            i *= v
            println(s"r == $r, i == $i for $v")
          }
        }

        println(r)
    }
  }
}
