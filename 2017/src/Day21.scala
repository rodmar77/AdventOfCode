import scala.io.Source

/*

  --- Day 21: Fractal Art ---

  You find a program trying to generate some art. It uses a strange process that
  involves repeatedly enhancing the detail of an image through a set of rules.

  The image consists of a two-dimensional square grid of pixels that are either
  on (#) or off (.). The program always begins with this pattern:

  .#.
  ..#
  ###

  Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have
  a size of 3.

  Then, the program repeats the following process:

  - If the size is evenly divisible by 2, break the pixels up into 2x2 squares,
    and convert each 2x2 square into a 3x3 square by following the corresponding
    enhancement rule.

  - Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3
    squares, and convert each 3x3 square into a 4x4 square by following the
    corresponding enhancement rule.

  Because each square of pixels is replaced by a larger one, the image gains
  pixels and so its size increases.

  The artist's book of enhancement rules is nearby (your puzzle input); however,
  it seems to be missing rules. The artist explains that sometimes, one must
  rotate or flip the input pattern to find a match. (Never rotate or flip the
  output pattern, though.) Each pattern is written concisely: rows are listed
  as single units, ordered top-down, and separated by slashes. For example, the
  following rules correspond to the adjacent patterns:

  +------------------------------+
  | ../.#  =  ..                 |
  |           .#                 |
  |                              |
  |                 .#.          |
  | .#./..#/###  =  ..#          |
  |                 ###          |
  |                              |
  |                         #..# |
  | #..#/..../#..#/.##.  =  .... |
  |                         #..# |
  |                         .##. |
  +------------------------------+

  When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns match the same rule:

  .#.   .#.   #..   ###
  ..#   #..   #.#   ..#
  ###   ###   ##.   .#.

  Suppose the book contained the following two rules:

  ../.# => ##./#../...
  .#./..#/### => #..#/..../..../#..#

  As before, the program begins with this pattern:

  .#.
  ..#
  ###

  The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the square matches the second rule, which produces:

  #..#
  ....
  ....
  #..#

  The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:

  #.|.#
  ..|..
  --+--
  ..|..
  #.|.#

  Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation to line up with the rule. The output for the rule is the same in all four cases:

  ##.|##.
  #..|#..
  ...|...
  ---+---
  ##.|##.
  #..|#..
  ...|...

  Finally, the squares are joined into a new grid:

  ##.##.
  #..#..
  ......
  ##.##.
  #..#..
  ......

  Thus, after 2 iterations, the grid contains 12 pixels that are on.

 */
object Day21 {

  def main(args: Array[String]): Unit = {
    val transformation = "(.+) => (.+)".r
    val transformations = Source
      .fromFile("inputs/2017/input_day21.txt")
      .getLines
      .map {
        case transformation(src, target) => Transformation(
          src
            .split("/")
            .toList,
          target
            .split("/")
            .toList)

      }.toList

    // How many pixels stay on after 5 iterations?
    println(iterate(".#./..#/###", transformations, 5).count(_ == '#'))

    // How many pixels stay on after 18 iterations?
    println(iterate(".#./..#/###", transformations, 18).count(_ == '#'))
  }

  case class Transformation(src: List[String], target: List[String]) {
    def flip(t: List[String]) = t.reverse
    def rotate(t: List[String], count: Int) = {
      def rot(acc: List[String], idx: Int): List[String] = {
        if (idx == count) acc
        else rot(acc.transpose.map(_.reverse.mkString), idx + 1)
      }

      rot(t, 0)
    }

    private val sources = List
      .tabulate(4)(n => List(
        rotate(src, n),
        flip(rotate(src, n))))
      .flatten
      .map(_.mkString("/"))

    def canTransform(v: String) = sources.contains(v)
  }

  def iterate(start: String, transformations: List[Transformation], rounds: Int) = {
    def split(board: String) = {
      def splitArea(ll: List[String], size: Int) = ll
        .map(_.sliding(size, size).toList)
        .transpose
        .map(_.mkString("/"))

      def split(board: List[String], size: Int) = board
        .sliding(size, size)
        .flatMap(splitArea(_, size))
        .toList

      val splitBoard = board.split("/").toList
      if (splitBoard.size % 2 == 0)
        split(splitBoard, 2)
      else
        split(splitBoard, 3)
    }

    def join(k: List[List[String]]) = {
      val size = Math.sqrt(k.size).toInt
      k
        .sliding(size, size)
        .map(_.transpose.map(_.mkString))
        .map(_.mkString("/"))
        .mkString("/")
    }

    def iterate(acc: String, current: Int): String = {
      if (current == rounds) acc
      else {
        val splits = split(acc).map(pt => transformations.find(_.canTransform(pt)) match {
          case Some(transf) => transf.target
       })

        iterate(
          join(splits),
          current + 1)
      }
    }

    transformations.find(_.canTransform(start)) match {
      case Some(transf) => iterate(transf.target.mkString("/"), 1)
    }
  }
}
