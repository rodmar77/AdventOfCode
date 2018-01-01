import scala.io.Source

/*

  --- Day 3: Perfectly Spherical Houses in a Vacuum ---

  Santa is delivering presents to an infinite two-dimensional grid of houses.

  He begins by delivering a present to the house at his starting location, and
  then an elf at the North Pole calls him via radio and tells him where to move
  next. Moves are always exactly one house to the north (^), south (v), east (>),
  or west (<). After each move, he delivers another present to the house at his
  new location.

  However, the elf back at the north pole has had a little too much eggnog, and
  so his directions are a little off, and Santa ends up visiting some houses more
  than once.

 */
object Day03 extends App {

  val text = Source.fromFile("inputs/2015/input_day03.txt").getLines.mkString

  /*

  How many houses receive at least one present?

  For example:

  > delivers presents to 2 houses: one at the starting location, and one to the east.

  ^>v< delivers presents to 4 houses in a square, including twice to the house at his
  starting/ending location.

  ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.

   */
  println(visited(text).size)

  /*
  --- Part Two ---

  The next year, to speed up the process, Santa creates a robot version of himself,
  Robo-Santa, to deliver presents with him.

  Santa and Robo-Santa start at the same location (delivering two presents to the
  same starting house), then take turns moving based on instructions from the elf,
  who is eggnoggedly reading from the same script as the previous year.

  This year, how many houses receive at least one present?

  For example:

  ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa
  goes south.

  ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where
  they started.

  ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and
  Robo-Santa going the other.
   */
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