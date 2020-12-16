import scala.io.Source
import scala.util.Using

object Day14 {

  val (maskExpression, valueExpression) = ("mask = ([01X]+)".r, """mem\[(\d+)] = (\d+)""".r)

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day14.txt")) {
      source =>
        val ll = source.getLines.toList

        println(process(ll).values.sum)
        println(processFloating(ll).values.sum)
    }
  }

  def process(ll: List[String]): Map[Int, Long] = {
    def applyMask(mask: String, number: Long): Long = {
      def applyMask(cm: String, cv: String, n: Long): Long = {
        if (cm.isEmpty) java.lang.Long.parseLong(cv, 2)
        else cm.last match {
          case 'X' => applyMask(cm.init, (n & 1) + cv, n >> 1)
          case digit => applyMask(cm.init, digit + cv, n >> 1)
        }
      }

      applyMask(mask, "", number)
    }

    def process(l: List[String], mask: String, acc: Map[Int, Long]): Map[Int, Long] = {
      if (l.isEmpty) acc
      else l.head match {
        case maskExpression(newMask) => process(l.tail, newMask, acc)
        case valueExpression(memory, value) => process(l.tail, mask, acc + (memory.toInt -> applyMask(mask, value.toLong)))
      }
    }

    process(ll, "", Map())
  }

  def processFloating(ll: List[String]): Map[Long, Long] = {
    def applyFloatingMask(mask: String, address: Long): List[Long] = {
      def applyFloatingMask(cm: String, cv: String, n: Long): List[Long] = {
        if (cm.isEmpty) List(java.lang.Long.parseLong(cv, 2))
        else cm.last match {
          case 'X' => (0 to 1).map(digit => applyFloatingMask(cm.init, digit + cv, n >> 1)).reduce(_ ++ _)
          case '0' => applyFloatingMask(cm.init, (n & 1) + cv, n >> 1)
          case  _  => applyFloatingMask(cm.init, 1 + cv, n >> 1)
        }
      }

      applyFloatingMask(mask, "", address)
    }

    def processFloatingMask(l: List[String], mask: String, acc: List[Map[Long, Long]]): List[Map[Long, Long]] = {
      if (l.isEmpty) acc
      else l.head match {
        case maskExpression(newMask) => processFloatingMask(l.tail, newMask, acc)
        case valueExpression(memory, value) => {
          val m = applyFloatingMask(mask, memory.toLong).map(nm => nm -> value.toLong).toMap
          processFloatingMask(l.tail, mask, acc :+ m)
        }
      }
    }

    processFloatingMask(ll, "", List()).foldRight(Map[Long, Long]())(_ ++ _)
  }
}
