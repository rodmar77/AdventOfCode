import scala.io.Source

object Day22 {

  def main(args: Array[String]): Unit = {
    val nodes = Source
      .fromFile("inputs/2016/input_day22.txt")
      .getLines
      .toList
      .drop(2)
      .map(getFileSystemNode)

    println(nodes.map(fn => nodes.count(isViablePair(fn, _))).sum)
    println(numberOfSteps(nodes))
  }

  def isViablePair(a: FileSystemNode, b: FileSystemNode): Boolean = a.used > 0 && a != b && a.used <= b.avail

  def getFileSystemNode(desc: String) = {
    val data = desc.split("\\s+")
    FileSystemNode(
      data(0).substring(data(0).lastIndexOf('/') + 1),
      data(1).init.toInt,
      data(2).init.toInt,
      data(3).init.toInt)
  }

  def numberOfSteps(nodes: List[FileSystemNode]) = {
    val (emptyNode, goal) = (
      nodes.find(_.used == 0).get,
      nodes.filter(_.y == 0).maxBy(_.x))

    val wallNodes = nodes.filter(_.size > emptyNode.size)
    val wallNodeWithMinX = wallNodes.minBy(_.x)

    (goal.x - 1) * 5 + emptyNode.y + wallNodes.size + (emptyNode.x - wallNodeWithMinX.x) + 1
  }

  case class FileSystemNode(name: String, size: Int, used: Int, avail: Int) {
    private val nameStructure = "node-x(\\d+)-y(\\d+)".r

    val x = name match { case nameStructure(a, _) => a.toInt }
    val y = name match { case nameStructure(_, b) => b.toInt }
  }
}
