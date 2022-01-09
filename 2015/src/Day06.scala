import java.lang.Math.max
import scala.io.Source
import scala.util.Using

/*
  --- Day 6: Probably a Fire Hazard ---

  Because your neighbors keep defeating you in the holiday house decorating
  contest year after year, you've decided to deploy one million lights in a
  1000x1000 grid.

  Furthermore, because you've been especially nice this year, Santa has mailed
  you instructions on how to display the ideal lighting configuration.

  Lights in your grid are numbered from 0 to 999 in each direction; the lights
  at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
  whether to turn on, turn off, or toggle various inclusive ranges given as
  coordinate pairs. Each coordinate pair represents opposite corners of a rectangle,
  inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in
  a 3x3 square. The lights all start turned off.

  To defeat your neighbors this year, all you have to do is set up your lights by
  doing the instructions Santa sent you in order.

  For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.

    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off
    the ones that were on, and turning on the ones that were off.

    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

  --- Part Two ---

  You just finish implementing your winning light pattern when you realize you
  mistranslated Santa's message from Ancient Nordic Elvish.

  The light grid you bought actually has individual brightness controls; each
  light can have a brightness of zero or more. The lights all start at zero.

    * The phrase turn on actually means that you should increase the brightness
      of those lights by 1.

    * The phrase turn off actually means that you should decrease the brightness
      of those lights by 1, to a minimum of zero.

    * The phrase toggle actually means that you should increase the brightness of
      those lights by 2.

  For example:

    turn on 0,0 through 0,0 would increase the total brightness by 1.
    toggle 0,0 through 999,999 would increase the total brightness by 2000000.
 */
object Day06 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2015/input_day06.txt")) {
      source =>
        val regex = """(.+?) (\d+),(\d+) through (\d+),(\d+)""".r

        val data = source
          .getLines
          .toList
          .map {
            case regex(a, x0, y0, x1, y1) => (a, (x0.toInt, y0.toInt), (x1.toInt, y1.toInt))
          }

        // After following the instructions, how many lights are lit?
        println(applyLights(0, data))

        //  What is the total brightness of all lights combined after following Santa's
        //  instructions?
        println(applyLights(1, data))
    }
  }


  def applyLights(lightType: Int, ls: List[(String, (Int, Int), (Int, Int))]): Int = {
    def apply(array: Seq[Seq[Int]], start: (Int, Int), end: (Int, Int), f: (Int, Int) => Int) = (start, end) match {
      case ((x0, y0), (x1, y1)) =>
        array.indices.collect(y => array(y).indices.collect(x => if (x0 <= x && x <= x1 && y0 <= y && y <= y1) f(x, y) else array(y)(x)))
    }

    def oldLights() = {
      val array = (0 until 1000).map(_ => (0 until 1000).map(_ => 0))
      ls.foldLeft(array) {
        case (array, (command, start, end)) =>
          command match {
            case "turn on" => apply(array, start, end, (_, _) => 1)
            case "turn off" => apply(array, start, end, (_, _) => 0)
            case "toggle" => apply(array, start, end, (x, y) => (array(y)(x) + 1) % 2)
          }
      }.flatten.sum
    }

    def newLights() = {
      val array = (0 until 1000).map(_ => (0 until 1000).map(_ => 0))
      ls.foldLeft(array) {
        case (array, (command, start, end)) =>
          command match {
            case "turn on" => apply(array, start, end, (x, y) => array(y)(x) + 1)
            case "turn off" => apply(array, start, end, (x, y) => (array(y)(x) - 1).max(0))
            case "toggle" => apply(array, start, end, (x, y) => array(y)(x) + 2)
          }
      }.flatten.sum
    }

    if (lightType == 0) oldLights() else newLights()
  }

}
