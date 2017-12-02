import scala.io.Source

object Day03 {

  def main(args: Array[String]): Unit = {
    val triangles = Source
      .fromFile("inputs/2016/input_day03.txt")
      .getLines
      .map(_.trim.split("\\s+").map(_.toInt))
      .toList

    val vertTriangles = triangles
      .sliding(3, 3)
      .map(arr => (0 to 2).map(y => (0 to 2).map(x => arr(x)(y))))
      .toList
      .flatten

    println(triangles.count(arr => arr.sum > 2 * arr.max))
    println(vertTriangles.count(arr => arr.sum > 2 * arr.max))

  }

}