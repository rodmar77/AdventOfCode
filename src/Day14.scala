import Math.{min, max}
import scala.io.Source

object Day14 extends App {

  val regex = """(.+?) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  val data = Source
    .fromFile("inputs/input_day14.txt")
    .getLines
    .toList
    .map {
      case regex(name, speed, fly, rest) => (name, speed.toInt, fly.toInt, rest.toInt)
    }

  println(data.map(position(_, 2503)).max)
  println(points(data.map(positions(_, 2503))).max)

  def points(ls: List[List[Int]]) = {
    def points(lls: List[Int]): List[Int] = lls.map(v => if (v == lls.max) 1 else 0)
    val ps = List.tabulate(ls.head.size, ls.size)((i, j) => ls(j)(i)).map(points)
    List.tabulate(ls.size, ls.head.size)((i, j) => ps(j)(i)).map(_.sum)
  }

  def position(n: (String, Int, Int, Int), s: Int) = n match {
    case (_, speed, fly, rest) => speed * ((s / (fly + rest)) * fly + min(fly, s % (fly + rest)))
  }

  def positions(n: (String, Int, Int, Int), s: Int) = n match {
    case (_, speed, fly, rest) =>
      def positions(r: Int, c: Int, acc: List[Int]): List[Int] = {
        if (r == 0) acc
        else positions(
          max(0, r - fly - rest),
          c + min(r, fly),
          acc
            ++ List.tabulate(min(r, fly))(i => (c + i + 1) * speed)
            ++ List.tabulate(max(min(r - fly, rest), 0))(i => (c + min(r, fly)) * speed))
      }

      positions(s, 0, List[Int]())
  }
}
