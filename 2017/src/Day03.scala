/*
  You come across an experimental new kind of memory stored on an infinite
  two-dimensional grid.

  Each square on the grid is allocated in a spiral pattern starting at a
  location marked 1 and then counting up while spiraling outward. For example,
  the first few squares are allocated like this:

  17  16  15  14  13
  18   5   4   3  12
  19   6   1   2  11
  20   7   8   9  10
  21  22  23---> ...

  While this is very space-efficient (no squares are skipped), requested data
  must be carried back to square 1 (the location of the only access port for
  this memory system) by programs that can only move up, down, left, or right.
  They always take the shortest path: the Manhattan Distance between the
  location of the data and square 1.

  For example:

    - Data from square 1 is carried 0 steps, since it's at the access port.
    - Data from square 12 is carried 3 steps, such as: down, left, left.
    - Data from square 23 is carried only 2 steps: up twice.
    - Data from square 1024 must be carried 31 steps.

 */

object Day03 {

  def main(args: Array[String]): Unit = {

    // How many steps are required to carry the data from the square identified
    // in your puzzle input all the way to the access port?
    println(dist((0, 0), pos(368078)))

    /*
      As a stress test on the system, the programs here clear the grid and then
      store the value 1 in square 1. Then, in the same allocation order as shown
      above, they store the sum of the values in all adjacent squares, including
      diagonals.

      So, the first few squares' values are chosen as follows:

        - Square 1 starts with the value 1.

        - Square 2 has only one adjacent filled square (with value 1), so it also
          stores 1.

        - Square 3 has both of the above squares as neighbors and stores the sum
          of their values, 2.

        - Square 4 has all three of the aforementioned squares as neighbors and
          stores the sum of their values, 4.

        - Square 5 only has the first and fourth squares as neighbors, so it gets
          the value 5.

      Once a square is written, its value does not change. Therefore, the first
      few squares would receive the following values:

      147  142  133  122   59
      304    5    4    2   57
      330   10    1    1   54
      351   11   23   25   26
      362  747  806--->   ...

      What is the first value written that is larger than your puzzle input?
     */
    println(generateAndFind(9, 368078))
  }

  def dist(a: (Int, Int), b: (Int, Int)): Int = (a._1 - b._1).abs + (a._2 - b._2).abs

  def pos(n: Int) = {
    val k = ((Math.sqrt(n) - 1) / 2).ceil.toInt
    val t = 2 * k + 1

    def pos(m: Int, t: Int): (Int, Int) = {
      if (n >= m - t) (k - (m - n), -k)
      else if (n >= m - 2 * t) (-k, -k + (m - t - n))
      else if (n >= m - 3 * t) (-k + (m - 2 * t - n), k)
      else (k, k - (m - n - 3 * t))
    }

    pos(t * t, t - 1)
  }

  def generateAndFind(w: Int, target: Int): Int = {
    abstract class Position(p: (Int, Int), m: List[List[Int]]) {
      def move: Position
      def updatedValue: Int = updated(p._2)(p._1)

      protected def updated: List[List[Int]] = m.updated(p._2, m(p._2).updated(p._1, getValue))
      protected def getValue: Int = (-1 to 1).flatMap(y => (-1 to 1).map(x => valueAt(p._1 + x, p._2 + y))).sum - m(p._2)(p._1)
      protected def canMove(x: Int, y: Int): Boolean = isDefinedAt(x, y) && valueAt(x, y) == 0

      private def isDefinedAt(x: Int, y: Int) = m.isDefinedAt(y) && m(y).isDefinedAt(x)
      private def valueAt(x: Int, y: Int) = if (isDefinedAt(x, y)) m(y)(x) else 0
    }

    case class Right(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1, p._2 - 1)) Up((p._1, p._2 - 1), updated)
      else Right((p._1 + 1, p._2), updated)
    }

    case class Left(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1, p._2 + 1)) Down((p._1, p._2 + 1), updated)
      else Left((p._1 - 1, p._2), updated)
    }

    case class Up(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1 - 1, p._2)) Left((p._1 - 1, p._2), updated)
      else Up((p._1, p._2 - 1), updated)
    }

    case class Down(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1 + 1, p._2)) Right((p._1 + 1, p._2), updated)
      else Down((p._1, p._2 + 1), updated)
    }

    def generate(c: Int, p: Position): Int = {
      if (p.updatedValue > target) p.updatedValue
      else generate(c + 1, p.move)
    }

    val (mi, acc) = ((w - 1) / 2, List.fill(w, w)(0))
    generate(1, Right((mi + 1, mi), acc.updated(mi, acc(mi).updated(mi, 1))))
  }
}
