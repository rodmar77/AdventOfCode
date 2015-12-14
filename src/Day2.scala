import scala.io.Source

object Day2 extends App {
  
  val lines = Source.fromFile("inputs/input_day02.txt").getLines.map(_.split("x").map(_.toInt).sorted).toList
  println(lines.map(n => 3*n(0)*n(1) + 2*n(1)*n(2) + 2*n(0)*n(2)).sum)
  println(lines.map(n => 2*(n(0)+n(1)) + (n(0)*n(1)*n(2))).sum)
  
}