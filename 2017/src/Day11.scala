import scala.io.Source

/*

  --- Day 11: Hex Ed ---

  Crossing the bridge, you've barely reached the other side of the stream when
  a program comes up to you, clearly in distress. "It's my child process," she
  says, "he's gotten lost in an infinite grid!"

  Fortunately for her, you have plenty of experience with infinite grids.

  Unfortunately for you, it's a hex grid.

  The hexagons ("hexes") in this grid are aligned such that adjacent hexes can
  be found to the north, northeast, southeast, south, southwest, and northwest:

    \ n  /
  nw +--+ ne
    /    \
  -+      +-
    \    /
  sw +--+ se
    / s  \

 */
object Day11 {

  def main(args: Array[String]): Unit = {
    val data = Source
      .fromFile("inputs/2017/input_day11.txt")
      .mkString
      .split(",")
      .toList

    data.foldLeft(((0, 0, 0), List(0))) {
      case (((x, y, z), ll), op) => op match {
        case "n"  => get(x, y + 1, z - 1, ll)
        case "s"  => get(x, y - 1, z + 1, ll)
        case "ne" => get(x + 1, y, z - 1, ll)
        case "sw" => get(x - 1, y, z + 1, ll)
        case "nw" => get(x - 1, y + 1, z, ll)
        case _    => get(x + 1, y - 1, z, ll)
      }
    } match {
      case (_, ll) =>
        // You have the path the child process took. Starting where he started,
        // you need to determine the fewest number of steps required to reach
        // him. (A "step" means to move from the hex you are in to any adjacent
        // hex.)
        println(ll.last)

        // How many steps away is the furthest he ever got from his starting
        // position?
        println(ll.max)
    }
  }

  def get(dx: Int, dy: Int, dz: Int, ll: List[Int]) = ((dx, dy, dz), ll :+ (dx.abs + dy.abs + dz.abs) / 2)
}
