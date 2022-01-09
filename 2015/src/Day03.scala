import scala.io.Source
import scala.util.Using

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

    > delivers presents to 2 houses: one at the starting location, and one to the east.
    ^>v< delivers presents to 4 houses in a square, including twice to the house at his
    starting/ending location.
    ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.

  --- Part Two ---

  The next year, to speed up the process, Santa creates a robot version of himself,
  Robo-Santa, to deliver presents with him.

  Santa and Robo-Santa start at the same location (delivering two presents to the
  same starting house), then take turns moving based on instructions from the elf,
  who is eggnoggedly reading from the same script as the previous year.

  For example:

    ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa
    goes south.
    ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where
    they started.
    ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and
    Robo-Santa going the other.

 */
object Day03 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2015/input_day03.txt")) {
      source =>
        val text = source.getLines.mkString

        //  How many houses receive at least one present?
        println(visited(text).size)

        //  This year, how many houses receive at least one present?
        val (even, odd) = text.indices.partition(_ % 2 == 0)
        println(
          (visited(text, even) ++ visited(text, odd))
            .distinct
            .size)
    }
  }

  def visited(text: String, s: Seq[Int]): Seq[(Int, Int)] = visited(s.map(text(_)).mkString)
  def visited(s: String) = s
    .scanLeft((0, 0))((a, b) => (a, b) match {
      case ((x, y), '^') => (x, y + 1)
      case ((x, y), 'v') => (x, y - 1)
      case ((x, y), '<') => (x - 1, y)
      case ((x, y), '>') => (x + 1, y)
    })
    .distinct
}