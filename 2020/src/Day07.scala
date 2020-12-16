import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day07 {

  case class Node(name: String, links: mutable.ArrayDeque[(Int, Node)]) {
      def addLink(cost: Int, n: Node) = links.append((cost, n))
  }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day07.txt")) {
      source => val bags = source.getLines
        .map(parseBagData)
        .toList

        println(countIncoming("shiny gold", bags))
        println(countOutgoing("shiny gold", bags))
    }
  }

  def countIncoming(name: String, bags: List[(String, List[(Int, String)])]) = {
    def getIncomingNodeMap() = {
      val acc = mutable.Map[String, Node]();
      bags.foreach {
        case (mainName, children) => {
          val mainNode = acc.getOrElseUpdate(mainName, Node(mainName, new mutable.ArrayDeque[(Int, Node)]))
          children.foreach {
            case (_, childName) => acc
              .getOrElseUpdate(
                  childName,
                  Node(childName, new mutable.ArrayDeque[(Int, Node)]))
              .addLink(0, mainNode)
          }
        }
      }

      acc
    }

    val m = getIncomingNodeMap()
    val (s, v) = (new mutable.Stack[Node]().push(m(name)), mutable.Set[String]())
    while (s.nonEmpty) {
      val top = s.pop()
      v += top.name
      top.links.foreach {
        case (_, node) => s.push(node)
      }
    }

    v.size - 1
  }

  def countOutgoing(name: String, bags: List[(String, List[(Int, String)])]) = {
    def countOutgoing(n: Node): Int = {
      if (n.links.isEmpty) 1
      else n.links.map {
        case (cost, child) => cost * countOutgoing(child)
      }.sum
    }

    def getOutgoingNodeMap() = {
      val acc = mutable.Map[String, Node]();
      bags.foreach {
        case (mainName, children) => {
          val mainNode = acc.getOrElseUpdate(mainName, Node(mainName, new mutable.ArrayDeque[(Int, Node)]))
          children.foreach {
            case (count, childName) => {
              mainNode.addLink(count, acc.getOrElseUpdate(childName, Node(childName, new mutable.ArrayDeque[(Int, Node)])))
            }
          }
        }
      }

      acc
    }

    countOutgoing(getOutgoingNodeMap()(name))
  }

  def parseBagData(line: String) = {
    val bagExpression = "(.+) bags contain".r
    val containExpression = """(\d+) ([^,.]+) (?:bag|bags)[,.]""".r

    (bagExpression.findFirstMatchIn(line).map(m => m.group(1)).get,
     containExpression.findAllMatchIn(line).map(m => (m.group(1).toInt, m.group(2))).toList)
  }
}

