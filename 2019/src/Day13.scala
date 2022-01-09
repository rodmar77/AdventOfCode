import util.IntCodeMachine

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

/*
  --- Day 13: Care Package ---

  As you ponder the solitude of space and the ever-increasing three-hour roundtrip for
  messages between you and Earth, you notice that the Space Mail Indicator Light is
  blinking. To help keep you sane, the Elves have sent you a care package.

  It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all the way
  on the other end of the ship. Surely, it won't be hard to build your own - the care
  package even comes with schematics.

  The arcade cabinet runs Intcode software like the game the Elves sent (your puzzle
  input). It has a primitive screen capable of drawing square tiles on a grid. The
  software draws tiles to the screen with output instructions: every three output
  instructions specify the x position (distance from the left), y position (distance
  from the top), and tile id. The tile id is interpreted as follows:

    0 is an empty tile. No game object appears in this tile.
    1 is a wall tile. Walls are indestructible barriers.
    2 is a block tile. Blocks can be broken by the ball.
    3 is a horizontal paddle tile. The paddle is indestructible.
    4 is a ball tile. The ball moves diagonally and bounces off objects.

  For example, a sequence of output values like 1,2,3,6,5,4 would draw a horizontal
  paddle tile (1 tile from the left and 2 tiles from the top) and a ball tile (6 tiles
  from the left and 5 tiles from the top).

  --- Part Two ---

  The game didn't run because you didn't put in any quarters. Unfortunately, you did not
  bring any quarters. Memory address 0 represents the number of quarters that have been
  inserted; set it to 2 to play for free.

  The arcade cabinet has a joystick that can move left and right. The software reads the
  position of the joystick with input instructions:

    If the joystick is in the neutral position, provide 0.
    If the joystick is tilted to the left, provide -1.
    If the joystick is tilted to the right, provide 1.

  The arcade cabinet also has a segment display capable of showing a single number that
  represents the player's current score. When three output instructions specify X=-1,
  Y=0, the third output instruction is not a tile; the value instead specifies the new
  score to show in the segment display. For example, a sequence of output values like
  -1,0,12345 would show 12345 as the player's current score.

 */
object Day13 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day13.txt")) {
      source =>
        val numbers = source
          .mkString
          .split(",")
          .map(BigInt(_))
          .toList

        // Start the game. How many block tiles are on the screen when the game exits?
        println(getBallCount(numbers))

        // Beat the game by breaking all the blocks. What is your score after the last
        // block is broken?
        try { println(playGame(numbers)) } catch { case e: Exception => e.printStackTrace()}
    }
  }

  def playGame(numbers: List[BigInt]): BigInt = {
    val lb = ListBuffer[BigInt]()
    val machine = IntCodeMachine(lb.append)

    @tailrec
    def doPlay(): BigInt = {
      if (machine.isRunning) {
        val positions = lb.sliding(3, 3).filter {
          case ListBuffer(_, _, item) => item == 3 || item == 4
        }.toList
         .sortBy {
           case ListBuffer(_, _, item) => item
        }.map {
           case ListBuffer(x, _, _) => x
        }

        lb.clear()
        val (px, bx) = if (positions.size == 1) (BigInt(0), positions.head) else (positions.head, positions.last)
        machine.resume(if (px > bx) -1 else if (px < bx) 1 else 0)
        doPlay()
      } else lb.max
    }

    machine.start(numbers.updated(0, BigInt(2)))
    doPlay()
  }

  def getBallCount(numbers: List[BigInt]): Int = {
    val lb = ListBuffer[BigInt]()
    val machine = IntCodeMachine(lb.append)
    machine.start(numbers)
    lb.sliding(3, 3).count {
      case ListBuffer(_, _, item) => item == 2
    }
  }

}