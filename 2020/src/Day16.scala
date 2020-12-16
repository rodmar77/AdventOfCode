import scala.io.Source
import scala.util.Using

object Day16 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day16.txt")) {
      source =>
        val (fields, myTicket, nearbyTickets) = process(source.getLines.toList)

        val ranges = fields.values.flatten.toList
        println(nearbyTickets.flatMap(_.filterNot(n => ranges.exists(_ contains n))).sum)

        val validTickets = nearbyTickets.filter(_.forall(n => ranges.exists(_ contains n)))
        val ticketFields = getTicketFields(validTickets, fields)

        println(
          ticketFields
            .zipWithIndex
            .filter {
              case (name, _) => name.startsWith("departure")
            }
            .map {
              case (_, index) => myTicket(index).toLong
            }
            .product)
    }
  }

  def process(ll: List[String]) = {
    val fieldsExpression = """([^:]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
    val fields = ll.takeWhile(_.matches(fieldsExpression.regex)).map {
      case fieldsExpression(field, sa, ea, sb, eb) => field -> List(sa.toInt to ea.toInt, sb.toInt to eb.toInt)
    }.toMap

    val myTickets = ll(fields.size + 2).split(",").map(_.toInt).toList
    val nearbyTickets = ll.drop(fields.size + 5).map(_.split(",").map(_.toInt).toList)

    (fields, myTickets, nearbyTickets)
  }

  def getTicketFields(validTickets: List[List[Int]], fields: Map[String, List[Range]]) = {
    def correctPossibilities(ll: List[List[String]]): List[String] = {
      if (ll.forall(_.size == 1)) ll.flatten
      else {
        val singleItems = ll.filter(_.size == 1).flatten.toSet
        correctPossibilities(ll.map(list =>
          if (list.size == 1) list
          else list.filterNot(singleItems)
        ))
      }
    }

    def getPossibilities(idx: Int, target: Int, acc: List[List[String]]): List[List[String]] = {
      if (idx == target) acc
      else {
        val ids = validTickets.map(_(idx))
        getPossibilities(
          idx + 1,
          target,
          acc :+ fields.filter {
            case (_, ranges) => ids.forall(n => ranges.exists(_ contains n))
          }.keySet.toList)
      }
    }

    correctPossibilities(getPossibilities(0, validTickets.head.size, List()))
  }

}
