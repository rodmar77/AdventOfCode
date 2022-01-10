import util.IntCodeMachine

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.language.implicitConversions
import scala.util.Using

/*
  --- Day 11: Space Police ---

  On the way to Jupiter, you're pulled over by the Space Police.

  "Attention, unmarked spacecraft! You are in violation of Space Law! All spacecraft must
  have a clearly visible registration identifier! You have 24 hours to comply or be sent
  to Space Jail!"

  Not wanting to be sent to Space Jail, you radio back to the Elves on Earth for help.
  Although it takes almost three hours for their reply signal to reach you, they send
  instructions for how to power up the emergency hull painting robot and even provide a
  small Intcode program (your puzzle input) that will cause it to paint your ship
  appropriately.

  There's just one problem: you don't have an emergency hull painting robot.

  You'll need to build a new emergency hull painting robot. The robot needs to be able to
  move around on the grid of square panels on the side of your ship, detect the color of
  its current panel, and paint its current panel black or white. (All of the panels are
  currently black.)

  The Intcode program will serve as the brain of the robot. The program uses input
  instructions to access the robot's camera: provide 0 if the robot is over a black panel
  or 1 if the robot is over a white panel. Then, the program will output two values:

  First, it will output a value indicating the color to paint the panel the robot is
  over: 0 means to paint the panel black, and 1 means to paint the panel white.
  Second, it will output a value indicating the direction the robot should turn: 0 means
  it should turn left 90 degrees, and 1 means it should turn right 90 degrees.

  After the robot turns, it should always move forward exactly one panel. The robot starts
  facing up.

  The robot will continue running for a while like this and halt when it is finished
  drawing. Do not restart the Intcode computer inside the robot during this process.

  For example, suppose the robot is about to start running. Drawing black panels as .,
  white panels as #, and the robot pointing the direction it is facing (< ^ > v), the
  initial state and region near the robot looks like this:

    .....
    .....
    ..^..
    .....
    .....

  The panel under the robot (not visible here because a ^ is shown instead) is also black,
  and so any input instructions at this point should be provided 0. Suppose the robot
  eventually outputs 1 (paint white) and then 0 (turn left). After taking these actions
  and moving forward one panel, the region now looks like this:

    .....
    .....
    .<#..
    .....
    .....

  Input instructions should still be provided 0. Next, the robot might output 0 (paint
  black) and then 0 (turn left):

    .....
    .....
    ..#..
    .v...
    .....

  After more outputs (1,0, 1,0):

    .....
    .....
    ..^..
    .##..
    .....

  The robot is now back where it started, but because it is now on a white panel, input
  instructions should be provided 1. After several more outputs (0,1, 1,0, 1,0), the
  area looks like this:

    .....
    ..<#.
    ...#.
    .##..
    .....

  Before you deploy the robot, you should probably have an estimate of the area it will
  cover: specifically, you need to know the number of panels it paints at least once,
  regardless of color. In the example above, the robot painted 6 panels at least once.
  (It painted its starting panel twice, but that panel is still only counted once; it
  also never painted the panel it ended on.)

  --- Part Two ---

  You're not sure what it's trying to paint, but it's definitely not a registration
  identifier. The Space Police are getting impatient.

  Checking your external ship cameras again, you notice a white panel marked "emergency
  hull painting robot starting panel". The rest of the panels are still black, but it
  looks like the robot was expecting to start on a white panel, not a black one.

 */
object Day11 {

  type PanelMap = Map[(Int, Int), Int]

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day11.txt")) {
      source =>
        val numbers = source
          .mkString
          .split(",")
          .map(BigInt(_))
          .toList

        // Build a new emergency hull painting robot and run the Intcode program on it.
        // How many panels does it paint at least once?
        println(getPanelCount(numbers))

        // Based on the Space Law Space Brochure that the Space Police attached to one of
        // your windows, a valid registration identifier is always eight capital letters.
        // After starting the robot on a single white panel instead, what registration
        // identifier does it paint on your hull?
        getRegistration(numbers).foreach(println)
    }
  }

  @tailrec
  def execute(machine: IntCodeMachine, buffer: ListBuffer[BigInt], currentDirection: Int, position: (Int, Int), panels: PanelMap): PanelMap = {
    if (machine.isRunning) {
      machine.resume(panels(position))
      buffer match {
        case ListBuffer(color, turn) =>
          val nextDirection = if (turn == 0) (currentDirection + 1) % 4 else if (currentDirection == 0) 3 else currentDirection - 1
          val nextPosition = (position, nextDirection) match {
            case ((x, y), 0) => (x, y + 1)
            case ((x, y), 1) => (x - 1, y)
            case ((x, y), 2) => (x, y - 1)
            case ((x, y), 3) => (x + 1, y)
          }

          buffer.clear()
          execute(machine, buffer, nextDirection, nextPosition, panels + (position -> color.intValue))
      }
    } else panels
  }

  def getPanelCount(p: List[BigInt]): Int = {
    val ll = ListBuffer.empty[BigInt]
    val machine = IntCodeMachine(ll.append)
    machine.start(p)

    val panels = execute(machine, ll, 0, (0, 0), Map().withDefaultValue(0))
    panels.size
  }

  def getRegistration(p: List[BigInt]): Seq[String] = {
    val ll = ListBuffer.empty[BigInt]
    val machine = IntCodeMachine(ll.append)
    machine.start(p)

    val panels = execute(machine, ll, 0, (0, 0), Map((0,0)->1).withDefaultValue(0))
    val ranges = panels.keys.foldLeft(((0, 0), (0, 0))) {
      case (((x0, x1), (y0, y1)), (x, y)) => ((x0 min x, x1 max x), (y0 min y, y1 max y))
    }

    val k = ranges match {
      case ((x0, x1), (y0, y1)) => (0 to y1 - y0).map(y => (0 to x1 - x0).map(x => panels((x + x0, y + y0))))
    }

    k
      .reverse
      .map(_
        .mkString
          .replace('1', '\u2589')
          .replace('0', ' '))
  }
}
