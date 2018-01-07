/*
  --- Day 13: A Maze of Twisty Little Cubicles ---

  You arrive at the first floor of this new building to discover a much less
  welcoming environment than the shiny atrium of the last one. Instead, you are
  in a maze of twisty little cubicles, all alike.

  Every location in this area is addressed by a pair of non-negative integers
  (x,y). Each such coordinate is either a wall or an open space. You can't move
  diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward
  positive x and y; negative values are invalid, as they represent a location
  outside the building. You are in a small waiting area at 1,1.

  While it seems chaotic, a nearby morale-boosting poster explains, the layout is
  actually quite logical. You can determine whether a given x,y coordinate will
  be a wall or an open space using a simple system:

    - Find x*x + 3*x + 2*x*y + y + y*y.
    - Add the office designer's favorite number (your puzzle input).
    - Find the binary representation of that sum; count the number of bits that
      are 1.
        - If the number of bits that are 1 is even, it's an open space.
        - If the number of bits that are 1 is odd, it's a wall.

  For example, if the office designer's favorite number were 10, drawing walls as
  # and open spaces as ., the corner of the building containing 0,0 would look
  like this:

    0123456789
  0 .#.####.##
  1 ..#..#...#
  2 #....##...
  3 ###.#.###.
  4 .##..#..#.
  5 ..##....#.
  6 #...##.###

  Now, suppose you wanted to reach 7,4. The shortest route you could take is marked
  as O:

    0123456789
  0 .#.####.##
  1 .O#..#...#
  2 #OOO.##...
  3 ###O#.###.
  4 .##OO#OO#.
  5 ..##OOO.#.
  6 #...##.###

  Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current
  location, 1,1).

 */
object Day13 {

  type Point = (Int, Int)

  def main(args: Array[String]): Unit = {
    // What is the fewest number of steps required for you to reach 31,39?
    shortestPath(1362, (1, 1), (31, 39)) match {
      case (length, _) => println(length)
    }

    // How many locations (distinct x,y coordinates, including your starting
    // location) can you reach in at most 50 steps?
    shortestPath(1362, (1, 1), (-1, -1), 50) match {
      case (_, visited) => println(visited.size)
    }
  }

  def shortestPath(favoriteNumber: Int, src: Point, dest: Point, maxDist: Int = Int.MaxValue) = {
    def bitCount(n: Int): Int = {
      def bitCount(n: Int, acc: Int): Int = {
        if (n == 0) acc
        else if ((n & 1) == 1) bitCount(n >> 1, acc + 1)
        else bitCount(n >> 1, acc)
      }

      bitCount(n, 0)
    }

    def validNeighbours(pt: Point) = pt match {
      case (x, y) => (-1 to 1).flatMap(inc => List((x + inc, y), (x, y + inc)))
        .filterNot {
          case (nx, ny) => nx == x && ny == y
        }.filter {
          case (nx, ny) => (nx >= 0) &&
            (ny >= 0) &&
            (bitCount(nx*(nx + 3 + 2*ny) + ny*(1 + ny) + favoriteNumber) % 2 == 0)
        }
    }

    def shortestPath(visited: Set[Point], curr: Point, dist: Int): (Int, Set[Point]) = {
      if (curr == dest || dist == maxDist) (dist, visited)
      else validNeighbours(curr)
              .filterNot(visited.contains)
              .map(np => shortestPath(visited + np, np, dist + 1))
              .reduceOption[(Int, Set[(Int, Int)])] {
                case ((a, as), (b, bs)) => (a.min(b), as ++ bs)
              }
              .getOrElse((Int.MaxValue, visited))
    }

    shortestPath(Set(src), src, 0)
  }
}
