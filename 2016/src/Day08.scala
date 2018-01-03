import scala.io.Source

/*
  --- Day 8: Two-Factor Authentication ---

  You come across a door implementing what you can only assume is an implementation
  of two-factor authentication after a long game of requirements telephone.

  To get past the door, you first swipe a keycard (no problem; there was one on
  a nearby desk). Then, it displays a code on a little screen, and you type that
  code on a keypad. Then, presumably, the door unlocks.

  Unfortunately, the screen has been smashed. After a few minutes, you've taken
  everything apart and figured out how it works. Now you just have to work out
  what the screen would have displayed.

  The magnetic strip on the card you swiped encodes a series of instructions for
  the screen; these instructions are your puzzle input. The screen is 50 pixels
  wide and 6 pixels tall, all of which start off, and is capable of three somewhat
  peculiar operations:

    * rect AxB turns on all of the pixels in a rectangle at the top-left of the
      screen which is A wide and B tall.

    * rotate row y=A by B shifts all of the pixels in row A (0 is the top row)
      right by B pixels. Pixels that would fall off the right end appear at the
      left end of the row.

    * rotate column x=A by B shifts all of the pixels in column A (0 is the left
      column) down by B pixels. Pixels that would fall off the bottom appear at
      the top of the column.

  For example, here is a simple sequence on a smaller screen:

    - [rect 3x2] creates a small rectangle in the top-left corner:

      ###....
      ###....
      .......

    - [rotate column x=1 by 1] rotates the second column down by one pixel:

      #.#....
      ###....
      .#.....

    - [rotate row y=0 by 4] rotates the top row right by four pixels:

      ....#.#
      ###....
      .#.....

    - [rotate column x=1 by 1] again rotates the second column down by one pixel,
      causing the bottom pixel to wrap back to the top:

      .#..#.#
      #.#....
      .#.....

  As you can see, this display technology is extremely powerful, and will soon
  dominate the tiny-code-displaying-screen market. That's what the advertisement
  on the back of the display tries to convince you, anyway.

 */
object Day08 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day08.txt")
      .getLines
      .toList

    val grid = createGrid(commands)

    // There seems to be an intermediate check of the voltage used by the display:
    // after you swipe your card, if the screen did work, how many pixels should
    // be lit?
    println(grid.map(_.count(_ == '#')).sum)

    // You notice that the screen is only capable of displaying capital letters;
    // in the font it uses, each letter is 5 pixels wide and 6 tall. After you
    // swipe your card, what code is the screen trying to display?
    println(grid.map(_.replace(".", " ")).mkString("\n"))
  }

  def createGrid(commands: List[String]) = {
    val (rect, rrow, rcol) = (
      "rect (\\d+)x(\\d+)".r,
      "rotate row y=(\\d+) by (\\d+)".r,
      "rotate column x=(\\d+) by (\\d+)".r
    )

    def createRect(w: Int, h: Int, grid: List[String]) = {
      def createRectCols(x: Int, acc: String): String = {
        if (x == w) acc
        else createRectCols(x + 1, acc.updated(x, '#'))
      }

      def createRectRow(y: Int, acc: List[String]): List[String] = {
        if (y == h) acc
        else createRectRow(y + 1, acc.updated(y, createRectCols(0, acc(y))))
      }

      createRectRow(0, grid)
    }

    def rotateByRow(row: Int, amount: Int, grid: List[String]) = {
      def rotateRow(col: Int, acc: String): String = {
        def colToGet = {
          val back = col - amount
          if (back < 0) back + acc.length else back
        }

        if (col == acc.length) acc
        else rotateRow(col + 1, acc.updated(col, grid(row)(colToGet)))
      }

      grid.updated(row, rotateRow(0, grid(row)))
    }

    def rotateByColumn(col: Int, amount: Int, grid: List[String]) = {
      def rotateCol(row: Int, acc: String): String = {
        def rowToGet = {
          val back = row - amount
          if (back < 0) back + grid.size else back
        }

        acc.updated(col, grid(rowToGet)(col))
      }

      def rotateRows(row: Int, acc: List[String]): List[String] = {
        if (row == grid.size) acc
        else rotateRows(row + 1, acc.updated(row, rotateCol(row, acc(row))))
      }

      rotateRows(0, grid)
    }

    def applyCommand(grid: List[String], c: String) = c match {
      case rect(w, h) => createRect(w.toInt, h.toInt, grid)
      case rrow(row, amount) => rotateByRow(row.toInt, amount.toInt, grid)
      case rcol(col, amount) => rotateByColumn(col.toInt, amount.toInt, grid)
    }

    commands.foldLeft(List.tabulate(6)(_ => "."*50))(applyCommand)
  }
}
