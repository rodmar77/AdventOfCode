import scala.io.Source

object Day07 {

  case class Node(name: String, weight: Int, children: List[String])

  def main(args: Array[String]): Unit = {
    val graph = Source
      .fromFile("inputs/2017/input_day07.txt")
      .getLines
      .map(splitLine)
      .toList

    val head = graph
      .filterNot(_.children.isEmpty)
      .find(p =>
        !graph
          .exists(ap =>
            ap != p && ap.children.contains(p.name)))
      .get

    println(getUnbalancedWeight(graph, head))
  }

  def getUnbalancedWeight(graph: List[Node], node: Node) = {
    def getNode(k: String) = graph.find(n => n.name.equals(k)).get
    def getWeights(node: Node) = {
      def getWeight(node: Node): Int = {
        def _getWeight(node: Node): Int = {
          if (node.children.isEmpty) node.weight
          else node.weight + node.children.map(n => _getWeight(getNode(n))).sum
        }

        _getWeight(node)
      }

      node.children.map(n => (n, getWeight(getNode(n))))
    }

    def getUnbalancedWeight(node: Node, acc: Int): Int = {
      def unbalancedNode(nodes: List[(String, Int)]): Option[Node] = {
        val grouped = nodes.groupBy(f => nodes.count(p => p._2 == f._2))
        if (grouped.contains(1)) Some(getNode(grouped(1).head._1))
        else None
      }

      def unbalancedDiff(nodes: List[(String, Int)]): Int = {
        val grouped = nodes.groupBy(f => nodes.count(p => p._2 == f._2))
        val (es, as) = (
          grouped(grouped.keys.max).head._2 * (grouped.keys.max + 1),
          nodes.map(_._2).sum)

        es - as
      }

      val weights = getWeights(node)
      val un = unbalancedNode(weights)

      if (un.isEmpty) node.weight + acc
      else getUnbalancedWeight(un.get, unbalancedDiff(weights))
    }

    getUnbalancedWeight(node, 0)
  }

  def splitLine(ll: String): Node = {
    def key: String = ll.substring(0, ll.indexOf(' '))
    def weight: Int = ll.substring(ll.indexOf('(') + 1, ll.indexOf(')')).toInt
    def values: List[String] = ll.split(" -> ")(1).split(", ").toList

    if (ll.contains("->")) Node(key, weight, values)
    else Node(key, weight, List())
  }

}
