import scala.io._

object Day9 extends App {

  val links = Source
    .fromFile("inputs/input_day09.txt")
    .getLines
    .map(_.split(" "))
    .map(arr => (arr(0), arr(2)) -> arr(4).toInt)
    .flatMap(p => Seq(p, (p._1.swap, p._2)))
    .toMap

  println(route(links).min)
  println(route(links).max)

  def route(vs: Map[(String, String), Int]) = {
    val names = vs.keys.map(_._1).toList.distinct
    names.permutations.map(_.sliding(2).map(l => vs((l.head, l.last))).sum)
  }
}