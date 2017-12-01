import scala.io.Source

object Day03 extends App {

  val text = Source.fromFile("inputs/2015/input_day03.txt").getLines.mkString

  println(visited(text).size)
  val (even, odd) = text.indices.partition(_ % 2 == 0)
  println(
    (visited(even) ++ visited(odd))
      .distinct
      .size)

  def visited(s: Seq[Int]): Seq[(Int, Int)] = visited(s.map(text(_)).mkString)
  def visited(s: String) = s
    .scanLeft((0, 0))((a, b) => b match {
      case '^' => (a._1, a._2 + 1)
      case 'v' => (a._1, a._2 - 1)
      case '<' => (a._1 - 1, a._2)
      case _ => (a._1 + 1, a._2)
    })
    .distinct
}