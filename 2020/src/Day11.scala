import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day11.txt")) {
      src => {
        val board = src.getLines.toList
        println(simpleStableCycle(board).map(_.count(_ == '#')).sum)
      }
    }
  }

  @tailrec
  def simpleStableCycle(board: List[String]): List[String] = {
    def neighborCount(row: Int, col: Int) = (-1 to 1).flatMap(nr => (-1 to 1).map(nc =>
        (nr != 0 || nc != 0) &&
        (board.isDefinedAt(row + nr) && board(row + nr).isDefinedAt(col + nc) && (board(row + nr)(col + nc) == '#')))
    ).count(_ == true)

    def cycleLine(line: String, i: Int): String = line.zipWithIndex.map {
      case (c, j) => c match {
        case 'L' => if (neighborCount(i, j) == 0) '#' else 'L'
        case '#' => if (neighborCount(i, j) >= 4) 'L' else '#'
        case any => any
      }
    }.mkString

    def cycle(): List[String] = board.zipWithIndex.map {
      case (line, idx) => cycleLine(line, idx)
    }

    val nextBoard = cycle()
    if (nextBoard.zipWithIndex.forall {
      case (line, index) => line.equals(board(index))
      case _ => false
    }) nextBoard
    else simpleStableCycle(nextBoard)
  }
}