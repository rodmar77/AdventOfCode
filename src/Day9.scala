import scala.io._

object Day9 extends App {

  val links = Source
    .fromFile("/tmp/input.txt")
    .getLines
    .map(_.split(" "))
    .map(arr => (arr(0), arr(2)) -> arr(4).toInt)
    .flatMap(p => List[((String, String), Int)](p, (p._1.swap, p._2)))
    .toMap

  println(route(links, _.min))
  println(route(links, _.max))

  def route(vs: Map[(String, String), Int], f: List[Int] => Int): Int = {
    def route(av: Map[(String, String), Int], curr: List[String], needed: Int, total: Int): Option[Int] = {
      if (curr.isEmpty) Option[Int](
        f(
          av
            .toList
            .map {
              case ((from, to), dist) => route(
                av.filter { case ((nextFrom, nextTo), _) => ((nextFrom != from) && (nextTo != from) && (nextTo != to)) },
                curr :+ from :+ to,
                av.keys.flatMap { case (from, to) => List[String](from, to) }.toList.distinct.size,
                dist)
            }
            .filter(_.isDefined)
            .map(_.get)))
            
      else if (curr.length == needed) (Option[Int](total))
      else if (av.isEmpty) (None)
      else Option[Int](
        f(
          av
            .toList
            .filter { case ((from, to), _) => (from == curr.last) }
            .map {
              case ((from, to), dist) => route(
                av.filter { case ((nextFrom, nextTo), _) => ((nextFrom != from) && (nextTo != from) && (nextTo != to)) },
                curr :+ to,
                needed,
                total + dist)
            }
            .filter(_.isDefined)
            .map(_.get)))
    }

    route(vs, List[String](), 0, 0).get
  }
}