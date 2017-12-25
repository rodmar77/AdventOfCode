import scala.io.Source

object Day12 {

  def main(args: Array[String]): Unit = {
    val data = Source
      .fromFile("inputs/2017/input_day12.txt")
      .getLines
      .map(_.split(" <-> "))
      .map(arr =>
        arr(0).toInt ->
        arr(1).split(", ").map(_.toInt))
      .toMap

    val joined = join(data)
    println(joined.count(_ == joined.head))
  }

  def join(data: Map[Int, Array[Int]]) = {
    def union(i: Int, j: Int, disjointSet: Array[Int]) = disjointSet(find(j, disjointSet)) = find(i, disjointSet)
    def find(i: Int, disjointSet: Array[Int]): Int = {
      if (i != disjointSet(i)) disjointSet(i) = find(disjointSet(i), disjointSet)
      disjointSet(i)
    }

    val result = Array.tabulate(data.size)(_.toInt)
    data.foreach {
      case (key, values) => values.foreach(union(key, _, result))
    }

    result.toList
  }

}
