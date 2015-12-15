import java.lang.Math.max
import scala.io.Source

object Day6 extends App {

  val regex = """(.+?) (\d+),(\d+) through (\d+),(\d+)""".r

  val data = Source
    .fromFile("inputs/input_day06.txt")
    .getLines
    .toList
    .map(regex.findFirstMatchIn(_).get)
    .map(m =>
      (m.group(1),
        (m.group(2).toInt, m.group(3).toInt),
        (m.group(4).toInt, m.group(5).toInt)))

  println(lightsOn(data))
  println(lightsToggle(data))

  def lightsOn(ls: List[(String, (Int, Int), (Int, Int))]) = {
    val array = Array.fill(1000, 1000)(false)
    ls.foreach {
      case (command, (x0, y0), (x1, y1)) =>
        command match {
          case "turn on" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) = true))
          case "turn off" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) = false))
          case "toggle" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) = !array(y)(x)))
        }
    }

    array.map(_.count(v => v)).sum
  }

  def lightsToggle(ls: List[(String, (Int, Int), (Int, Int))]) = {
    val array = Array.fill(1000, 1000)(0)
    ls.foreach {
      case (command, (x0, y0), (x1, y1)) =>
        command match {
          case "turn on" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) += 1))
          case "turn off" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) = max(array(y)(x) - 1, 0)))
          case "toggle" => (x0 to x1).foreach(x => (y0 to y1).foreach(y => array(y)(x) += 2))
        }
    }

    array.map(_.sum).sum
  }

}
