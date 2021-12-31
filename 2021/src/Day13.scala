import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

/*
  --- Day 13: Transparent Origami ---

  You reach another volcanically active part of the cave. It would be nice if you could do
  some kind of thermal imaging so you could tell ahead of time which caves are too hot to
  safely enter.

  Fortunately, the submarine seems to be equipped with a thermal camera! When you activate
  it, you are greeted with:

  Congratulations on your purchase! To activate this infrared thermal imaging
  camera system, please enter the code found on page 1 of the manual.
  Apparently, the Elves have never used this feature. To your surprise, you manage to find
  the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent
  paper! The transparent paper is marked with random dots and includes instructions on how
  to fold it up (your puzzle input). For example:

    6,10
    0,14
    9,10
    0,3
    10,4
    4,11
    6,0
    6,12
    4,1
    0,13
    10,12
    3,4
    3,0
    8,4
    1,10
    2,14
    8,10
    9,0

    fold along y=7
    fold along x=5

  The first section is a list of dots on the transparent paper. 0,0 represents the top-left
  coordinate. The first value, x, increases to the right. The second value, y, increases
  downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below
  0,0. The coordinates in this example form the following pattern, where # is a dot on the
  paper and . is an empty, unmarked position:

    ...#..#..#.
    ....#......
    ...........
    #..........
    ...#....#.#
    ...........
    ...........
    ...........
    ...........
    ...........
    .#....#.##.
    ....#......
    ......#...#
    #..........
    #.#........

  Then, there is a list of fold instructions. Each instruction indicates a line on the
  transparent paper and wants you to fold the paper up (for horizontal y=... lines) or
  left (for vertical x=... lines). In this example, the first fold instruction is fold
  along y=7, which designates the line formed by all of the positions where y is 7
  (marked here with -):

    ...#..#..#.
    ....#......
    ...........
    #..........
    ...#....#.#
    ...........
    ...........
    -----------
    ...........
    ...........
    .#....#.##.
    ....#......
    ......#...#
    #..........
    #.#........

  Because this is a horizontal line, fold the bottom half up. Some of the dots might end
  up overlapping after the fold is complete, but dots will never appear exactly on a
  fold line. The result of doing this fold looks like this:

    #.##..#..#.
    #...#......
    ......#...#
    #...#......
    .#.#..#.###
    ...........
    ...........

  Now, only 17 dots are visible.

  Notice, for example, the two dots in the bottom left corner before the transparent paper
  is folded; after the fold is complete, those dots appear in the top left corner (at 0,0
  and 0,1). Because the paper is transparent, the dot just below them in the result (at
  0,3) remains visible, as it can be seen through the transparent paper.

  Also notice that some dots can end up overlapping; in this case, the dots merge together
  and become a single dot.

  The second fold instruction is fold along x=5, which indicates this line:

    #.##.|#..#.
    #...#|.....
    .....|#...#
    #...#|.....
    .#.#.|#.###
    .....|.....
    .....|.....

  Because this is a vertical line, fold left:

    #####
    #...#
    #...#
    #...#
    #####
    .....
    .....

  The instructions made a square!

  The transparent paper is pretty big, so for now, focus on just completing the first fold.
  After the first fold in the example above, 17 dots are visible - dots that end up
  overlapping after the fold is completed count as a single dot.

  --- Part Two ---

  Finish folding the transparent paper according to the instructions. The manual says the
  code is always eight capital letters.

 */
object Day13 {

  type Paper = List[List[Char]]

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day13.txt")) {
      source =>
        val lines = source.getLines()
        val coords = lines
          .takeWhile(_.matches("\\d+,\\d+"))
          .map(_.split(","))
          .map {
            case Array(a, b) => (a.toInt, b.toInt)
          }.toList

        val instructions = lines.toList

        //  How many dots are visible after completing just the first fold instruction on
        //  your transparent paper?
        val firstFold = foldPaper(extractPaper(coords), instructions.head)
        println(firstFold.flatten.count(_ == '#'))

        // What code do you use to activate the infrared thermal imaging camera system?
        val foldedPaper = instructions.tail.foldLeft(firstFold) {
          case (paper, instruction) => foldPaper(paper, instruction)
        }

        foldedPaper.foreach(n => println(n.mkString))

    }
  }

  def foldPaper(paper: Paper, instruction: String): Paper = {
    val foldExpression = "fold along (.)=(\\d+)".r
    def executeFold(paper: Paper, instruction: String): Paper = instruction match {
      case foldExpression(axis, position) => axis match {
        case "x" => foldVertical(paper, position.toInt)
        case "y" => foldHorizontal(paper, position.toInt)
      }
    }

    def add(p1: Paper, p2: Paper): Paper = {
      if (p1.length == p2.length) p1.indices.toList.map(y => p1(y).indices.toList.map(x => {
          if (p1(y)(x)== '#' || p2(y)(x) == '#') '#' else ' '
        }))
      else p1.splitAt(p1.length - p2.length) match {
          case (up, down) => up ++ add(down, p2)
        }
    }

    def foldVertical(paper: Paper, position: Int): Paper = paper.transpose.splitAt(position) match {
      case (up, down) => add(up.transpose, down.tail.reverse.transpose)
    }

    def foldHorizontal(paper: Paper, position: Int): List[List[Char]] = paper.splitAt(position) match {
      case (up, down) => add(up, down.tail.reverse)
    }

    executeFold(paper, instruction)
  }

  def extractPaper(coords: List[(Int, Int)]): Paper = {
    val (width, height) = (
      coords.map { case (x, _) => x }.max + 1,
      coords.map { case (_, y) => y }.max + 1)

    coords.foldLeft(List.fill(height)(List.fill(width)('.'))) {
      case (p, (x, y)) => p.updated(y, p(y).updated(x, '#'))
    }
  }
}
