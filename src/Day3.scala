import scala.io.Source

object Day3 extends App {

  val text = Source.fromFile("inputs/input_day03.txt").getLines.mkString

  println(visited(text).size)
  println(
    (visited(filterBy(_ % 2 == 0)) ++
      visited(filterBy(_ % 2 == 1)))
      .distinct
      .size)

  def filterBy(f: Int => Boolean) = text.zipWithIndex.filter(p => f(p._2)).map(_._1).mkString
  def visited(s: String) = s
    .scanLeft((0, 0))((a, b) => b match {
      case '^' => (a._1, a._2 + 1)
      case 'v' => (a._1, a._2 - 1)
      case '<' => (a._1 - 1, a._2)
      case _ => (a._1 + 1, a._2)
    })
    .distinct
}