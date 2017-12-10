import scala.io.Source

object Day20 {

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromFile("inputs/2016/input_day20.txt")
      .getLines
      .toList
      .map(_.split("-"))
      .map(arr => (arr(0).toLong, arr(1).toLong))
      .sorted
      .foldLeft(List[(Long, Long)]()) { (acc, t) =>
      acc match {
        case x :: xs if x._2 >= t._1 => (x._1, math.max(x._2, t._2)) :: xs
        case x :: xs => t +: acc
        case _ => List(t)
      }
    }.reverse


    // getFirstAllowed(lines)
    println(lines.sliding(2).map(k => (k.head._2, k.last._1)).find(p => p._2 - p._1 > 1).get._1 + 1)
    println(lines.sliding(2).map(k => k.last._1 - k.head._2 - 1).sum)
  }

}
