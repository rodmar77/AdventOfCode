import scala.io.Source

object Day01 {

  abstract class Position(p: (Int, Int)) {

    val dataPattern = "(\\w)(\\d+)".r

    def getPosition: (Int, Int) = p
    def move(np: Position, count: Int): Position
    def distanceFromStart: Int = p._1.abs + p._2.abs
    def move(np: String): Position = {
      val dataPattern(d, c) = np

      if (d.equals("L")) move(Left(p), c.toInt)
      else move(Right(p), c.toInt)
    }
  }

  case class Right(p: (Int, Int)) extends Position(p) {
    def move(np: Position, c: Int): Position = np match {
      case _: Right => Down((p._1, p._2 - c))
      case _: Left => Up((p._1, p._2 + c))
      case _: Down => Left((p._1 - c, p._2))
      case _ => Right((p._1 + c, p._2))
    }
  }

  case class Left(p: (Int, Int)) extends Position(p) {
    def move(np: Position, c: Int): Position = np match {
      case _: Right => Up((p._1, p._2 + c))
      case _: Left => Down((p._1, p._2 - c))
      case _: Down => Right((p._1 + c, p._2))
      case _ => Left((p._1 - c, p._2))
    }
  }

  case class Up(p: (Int, Int)) extends Position(p) {
    def move(np: Position, c: Int): Position = np match {
      case _: Right => Right((p._1 + c, p._2))
      case _: Left => Left((p._1 - c, p._2))
      case _: Down => Down((p._1, p._2 - c))
      case _ => Up((p._1, p._2 + c))
    }
  }

  case class Down(p: (Int, Int)) extends Position(p) {
    def move(np: Position, c: Int): Position = np match {
      case _: Right => Left((p._1 - c, p._2))
      case _: Left => Right((p._1 + c, p._2))
      case _: Down => Up((p._1, p._2 + c))
      case _ => Down((p._1, p._2 - c))
    }
  }

  def moveUntilAnyPositionIsVisitedAgain(codes: List[String]) = {
    def getAllSteps(f: (Int, Int), t: (Int, Int)) = {
      if (f._1 == t._1 && f._2 > t._2) (f._2 - 1 to t._2 by -1).map((f._1, _))
      else if (f._1 == t._1) (f._2 + 1 to t._2).map((f._1, _))
      else if (f._1 > t._1) (f._1 - 1 to t._1 by -1).map((_, f._2))
      else (f._1 + 1 until t._1).map((_, f._2))
    }

    def moveUntilAnyPositionIsVisitedAgain(visited: List[(Int, Int)], toVisit: List[String], p: Position): Position = {
      if (toVisit.isEmpty) p
      else {
        val nextPosition = p.move(toVisit.head)
        val allSteps = getAllSteps(p.getPosition, nextPosition.getPosition)

        val duplicateStep = allSteps.find(visited.contains)
        if (duplicateStep.isDefined) Up(duplicateStep.get)
        else moveUntilAnyPositionIsVisitedAgain(visited ++ allSteps, toVisit.tail, nextPosition)
      }
    }

    moveUntilAnyPositionIsVisitedAgain(List((0, 0)), codes, Up((0, 0)))
  }

  def main(args: Array[String]): Unit = {
    val codes = Source
      .fromFile("inputs/2016/input_day01.txt")
      .getLines
      .mkString
      .split(",")
      .toList
      .map(_.trim)

    val start: Position = Up((0, 0))
    println(codes.foldLeft(start)((a, b) => a.move(b)).distanceFromStart)
    println(moveUntilAnyPositionIsVisitedAgain(codes).distanceFromStart)
  }
}
