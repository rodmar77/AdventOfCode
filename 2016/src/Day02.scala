import scala.io.Source

object Day02 {

  case class Position(x: Int, y: Int, map: List[String]) {
    def move(direction: Char): Position = {
      if (direction == 'R')      move(x+1, y)
      else if (direction == 'L') move(x-1, y)
      else if (direction == 'U') move(x, y-1)
      else                       move(x, y+1)
    }

    def canMove(ix: Int, iy: Int) = map.isDefinedAt(iy) && map(iy).isDefinedAt(ix) && (map(iy)(ix) != ' ')
    def move(ix: Int, iy: Int) = if (canMove(ix, iy)) Position(ix, iy, map) else this
    def getValue = map(y)(x)
  }

  def main(args: Array[String]): Unit = {
    val codes = Source.fromFile("inputs/2016/input_day02.txt").getLines.toList

    println(getCode(
     codes,
     1, 1,
     List(
       "123",
       "456",
       "789")))

    println(getCode(
      codes,
      0, 2,
      List(
        "  1  ",
        " 234 ",
        "56789",
        " ABC ",
        "  D  ")))
  }

  def getCode(codes: List[String], x: Int, y: Int, map: List[String]) = {
    def nextPosition(s: String, p: Position) = s.foldLeft(p)((a, b) => a.move(b))
    def getCode(remCodes: List[String], code: String, p: Position): String = {
      if (remCodes.isEmpty) code
      else {
        val np = nextPosition(remCodes.head, p)
        getCode(remCodes.tail, code + np.getValue, np)
      }
    }

    getCode(codes, "", Position(x, y, map))
  }

}