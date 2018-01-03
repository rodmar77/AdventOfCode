import scala.io.Source

/*

  --- Day 1: No Time for a Taxicab ---

  Santa's sleigh uses a very high-precision clock to guide its movements, and
  the clock's oscillator is regulated by stars. Unfortunately, the stars have
  been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
  retrieve all fifty stars by December 25th.

  Collect stars by solving puzzles. Two puzzles will be made available on each
  day in the advent calendar; the second puzzle is unlocked when you complete
  the first. Each puzzle grants one star. Good luck!

  You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
  unfortunately, is as close as you can get - the instructions on the Easter
  Bunny Recruiting Document the Elves intercepted start here, and nobody had
  time to work them out further.

  The Document indicates that you should start at the given coordinates (where
  you just landed) and face North. Then, follow the provided sequence: either
  turn left (L) or right (R) 90 degrees, then walk forward the given number of
  blocks, ending at a new intersection.

  There's no time to follow such ridiculous instructions on foot, though, so you
  take a moment and work out the destination. Given that you can only walk on
  the street grid of the city, how far is the shortest path to the destination?

  For example:

      * Following [R2, L3] leaves you 2 blocks East and 3 blocks North, or 5
        blocks away.

      * [R2, R2, R2] leaves you 2 blocks due South of your starting position,
        which is 2 blocks away.

      * [R5, L5, R5, R3] leaves you 12 blocks away.

 */
object Day01 {

  abstract class Position(p: (Int, Int)) {

    private val dataPattern = "(\\w)(\\d+)".r

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

    // How many blocks away is Easter Bunny HQ?
    val start: Position = Up((0, 0))
    println(codes.foldLeft(start)((a, b) => a.move(b)).distanceFromStart)

    /*

    Then, you notice the instructions continue on the back of the Recruiting
    Document. Easter Bunny HQ is actually at the first location you visit twice.

    For example, if your instructions are R8, R4, R4, R8, the first location
    you visit twice is 4 blocks away, due East.

    How many blocks away is the first location you visit twice?

     */
    println(moveUntilAnyPositionIsVisitedAgain(codes).distanceFromStart)
  }
}
