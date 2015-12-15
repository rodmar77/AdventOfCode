import scala.io.Source

object Day1 extends App {

  val text = Source.fromFile("inputs/input_day01.txt").getLines.mkString
  val values = Map('(' -> 1, ')' -> -1)

  println(text.map(values).sum)
  println(text.map(values).scanLeft(0)(_ + _).indexWhere(_ < 0))
  
}