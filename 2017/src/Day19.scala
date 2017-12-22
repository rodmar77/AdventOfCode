import scala.io.Source

object Day19 {

  def main(args: Array[String]): Unit = {
    val grid = Source
      .fromFile("inputs/2017/input_day19.txt")
      .getLines
      .toList

    println(getTextFromGrid(grid))
  }

  def getTextFromGrid(grid: List[String]) = {
    object Direction extends Enumeration {
      type Direction = Value
      val Up, Down, Left, Right = Value
    }

    import Direction._
    case class Position(x: Int, y: Int) {
      def next(d: Direction): Position = d match {
        case Up => Position(x, y - 1)
        case Down => Position(x, y + 1)
        case Left => Position(x - 1, y)
        case _ => Position(x + 1, y)
      }
    }

    def valueAt(pos: Position) = grid(pos.y)(pos.x)

    def canWalkTo(pos: Position): Boolean = grid.isDefinedAt(pos.y) &&
      grid(pos.y).isDefinedAt(pos.x) &&
      !grid(pos.y)(pos.x).isSpaceChar

    def walkNextDirection(pos: Position, direction: Direction, acc: String, total: Int) = direction match {
      case Up | Down => if (canWalkTo(pos.next(Left)))
        walk(pos.next(Left), Left, acc, total + 1)
      else
        walk(pos.next(Right), Right, acc, total + 1)

      case Left | Right => if (canWalkTo(pos.next(Up)))
        walk(pos.next(Up), Up, acc, total + 1)
      else
        walk(pos.next(Down), Down, acc, total + 1)
    }

    def walk(pos: Position, direction: Direction, acc: String, total: Int): (String, Int) = {
      val c = valueAt(pos)

      if (c.isSpaceChar) (acc, total)
      else if (c.isLetter) walk(pos.next(direction), direction, acc + c, total + 1)
      else if (c == '+') walkNextDirection(pos, direction, acc, total)
      else walk(pos.next(direction), direction, acc, total + 1)
    }

    walk(Position(grid.head.indexOf('|'), 0), Down, "", 0)
  }
}
