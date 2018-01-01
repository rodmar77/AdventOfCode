import java.lang.Math.{max, min}

import scala.io.Source

/*
  --- Day 14: Reindeer Olympics ---

  This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
  rest occasionally to recover their energy. Santa would like to know which of
  his reindeer is fastest, and so he has them race.

  Reindeer can only either be flying (always at their top speed) or resting (not
  moving at all), and always spend whole seconds in either state.

  For example, suppose you have the following Reindeer:

  * Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
  * Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

  After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten
  seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh
  second, Comet begins resting (staying at 140 km), and Dancer continues on for
  a total distance of 176 km. On the 12th second, both reindeer are resting.

  They continue to rest until the 138th second, when Comet flies for another ten
  seconds. On the 174th second, Dancer flies for another 11 seconds.

  In this example, after the 1000th second, both reindeer are resting, and Comet
  is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
  So, in this situation, Comet would win (if the race ended at 1000 seconds).

 */
object Day14 extends App {

  val regex = """(.+?) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  val data = Source
    .fromFile("inputs/2015/input_day14.txt")
    .getLines
    .toList
    .map {
      case regex(name, speed, fly, rest) => (name, speed.toInt, fly.toInt, rest.toInt)
    }

  // Given the descriptions of each reindeer (in your puzzle input), after exactly
  // 2503 seconds, what distance has the winning reindeer traveled?
  println(data.map(position(_, 2503)).max)

  /*
  Seeing how reindeer move in bursts, Santa decides he's not pleased with the old
  scoring system.

  Instead, at the end of each second, he awards one point to the reindeer currently
  in the lead. (If there are multiple reindeer tied for the lead, they each get one
  point.) He keeps the traditional 2503 second time limit, of course, as doing
  otherwise would be entirely ridiculous.

  Given the example reindeer from above, after the first second, Dancer is in the
  lead and gets one point. He stays in the lead until several seconds into Comet's
  second burst: after the 140th second, Comet pulls into the lead and gets his first
  point. Of course, since Dancer had been in the lead for the 139 seconds before
  that, he has accumulated 139 points by the 140th second.

  After the 1000th second, Dancer has accumulated 689 points, while poor Comet, our
  old champion, only has 312. So, with the new scoring system, Dancer would win
  (if the race ended at 1000 seconds).

  Again given the descriptions of each reindeer (in your puzzle input), after
  exactly 2503 seconds, how many points does the winning reindeer have?
   */
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
            ++ List.tabulate(max(min(r - fly, rest), 0))(_ => (c + min(r, fly)) * speed))
      }

      positions(s, 0, List())
  }
}
