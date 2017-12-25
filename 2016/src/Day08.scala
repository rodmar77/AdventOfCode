import scala.io.Source

object Day08 {

  def main(args: Array[String]): Unit = {
    val commands = Source
      .fromFile("inputs/2016/input_day08.txt")
      .getLines
      .toList

    val grid = createGrid(commands)
    println(grid.map(_.count(_ == '#')).sum)
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
