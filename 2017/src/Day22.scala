import scala.collection.mutable.ListBuffer
import scala.io.Source

/*

  --- Day 22: Sporifica Virus ---

  Diagnostics indicate that the local grid computing cluster has been contaminated
  with the Sporifica Virus. The grid computing cluster is a seemingly-infinite
  two-dimensional grid of compute nodes. Each node is either clean or infected by
  the virus.

  To prevent overloading the nodes (which would render them useless to the virus)
  or detection by system administrators, exactly one virus carrier moves through
  the network, infecting or cleaning nodes as it moves. The virus carrier is always
  located on a single node in the network (the current node) and keeps track of
  the direction it is facing.

  To avoid detection, the virus carrier works in bursts; in each burst, it wakes
  up, does some work, and goes back to sleep. The following steps are all executed
  in order one time each burst:

  - If the current node is infected, it turns to its right. Otherwise, it turns to
    its left. (Turning is done in-place; the current node does not change.)

  - If the current node is clean, it becomes infected. Otherwise, it becomes cleaned.
    (This is done after the node is considered for the purposes of changing direction.)
    The virus carrier moves forward one node in the direction it is facing.

  Diagnostics have also provided a map of the node infection status (your puzzle
  input). Clean nodes are shown as .; infected nodes are shown as #. This map only
  shows the center of the grid; there are many more nodes beyond those shown, but
  none of them are currently infected.

  The virus carrier begins in the middle of the map facing up.

  For example, suppose you are given a map like this:

  ..#
  #..
  ...

  Then, the middle of the infinite grid looks like this, with the virus carrier's
  position marked with [ ]:

  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . # . . .
  . . . #[.]. . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  The virus carrier is on a clean node, so it turns left, infects the node, and
  moves left:

  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . # . . .
  . . .[#]# . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  The virus carrier is on an infected node, so it turns right, cleans the node,
  and moves up:

  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . .[.]. # . . .
  . . . . # . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  Four times in a row, the virus carrier finds a clean, infects it, turns left,
  and moves forward, ending in the same place and still facing up:

  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . #[#]. # . . .
  . . # # # . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  Now on the same node as before, it sees an infection, which causes it to turn
  right, clean the node, and move forward:

  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . # .[.]# . . .
  . . # # # . . . .
  . . . . . . . . .
  . . . . . . . . .
  . . . . . . . . .
  After the above actions, a total of 7 bursts of activity had taken place. Of
  them, 5 bursts of activity caused an infection.

  After a total of 70, the grid looks like this, with the virus carrier facing up:

  . . . . . # # . .
  . . . . # . . # .
  . . . # . . . . #
  . . # . #[.]. . #
  . . # . # . . # .
  . . . . . # # . .
  . . . . . . . . .
  . . . . . . . . .
  By this time, 41 bursts of activity caused an infection (though most of those
  nodes have since been cleaned).

  After a total of 10000 bursts of activity, 5587 bursts will have caused an
  infection.

 */
object Day22 {

  def main(args: Array[String]): Unit = {
    val infected = Source
      .fromFile("inputs/2017/input_day22.txt")
      .getLines
      .map(_
        .toList
        .zipWithIndex
        .filter {
          case (c, _) => c == '#'
        }.map {
        case (_, x) => x
      })
      .toList
      .zipWithIndex
      .flatMap {
        case (ll, y) => ll.map((_, y))
      }

    // Given your actual map, after 10000 bursts of activity, how many bursts
    // cause a node to become infected? (Do not count nodes that begin infected.)
    println(burst(infected, 10000, evolved = false).getAffected())

    /*

    As you go to remove the virus from the infected nodes, it evolves to resist
    your attempt.

    Now, before it infects a clean node, it will weaken it to disable your defenses.
    If it encounters an infected node, it will instead flag the node to be cleaned
    in the future. So:

    - Clean nodes become weakened.
    - Weakened nodes become infected.
    - Infected nodes become flagged.
    - Flagged nodes become clean.

    Every node is always in exactly one of the above states.

    The virus carrier still functions in a similar way, but now uses the following
    logic during its bursts of action:

    - Decide which way to turn based on the current node:
      - If it is clean, it turns left.
      - If it is weakened, it does not turn, and will continue moving in the same
        direction.
      - If it is infected, it turns right.
      - If it is flagged, it reverses direction, and will go back the way it came.
    - Modify the state of the current node, as described above.

    The virus carrier moves forward one node in the direction it is facing.
    Start with the same map (still using . for clean and # for infected) and still
    with the virus carrier starting in the middle and facing up.

    Using the same initial state as the previous example, and drawing weakened
    as W and flagged as F, the middle of the infinite grid looks like this,
    with the virus carrier's position again marked with [ ]:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . # . . .
    . . . #[.]. . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    This is the same as before, since no initial nodes are weakened or flagged.
    The virus carrier is on a clean node, so it still turns left, instead weakens
    the node, and moves left:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . # . . .
    . . .[#]W . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    The virus carrier is on an infected node, so it still turns right, instead
    flags the node, and moves up:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . .[.]. # . . .
    . . . F W . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    This process repeats three more times, ending on the previously-flagged node
    and facing right:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . W W . # . . .
    . . W[F]W . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    Finding a flagged node, it reverses direction and cleans the node:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . W W . # . . .
    . .[W]. W . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    The weakened node becomes infected, and it continues in the same direction:

    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . W W . # . . .
    .[.]# . W . . . .
    . . . . . . . . .
    . . . . . . . . .
    . . . . . . . . .

    Of the first 100 bursts, 26 will result in infection. Unfortunately, another
    feature of this evolved virus is speed; of the first 10000000 bursts, 2511944
    will result in infection.

    Given your actual map, after 10000000 bursts of activity, how many bursts cause
    a node to become infected? (Do not count nodes that begin infected.)
     */
    println(burst(infected, 10000000, evolved = true).getAffected())
  }

  abstract class Virus {
    def burst(): Virus
    def getAffected(): Int
  }

  def burst(infected: List[(Int, Int)], c: Int, evolved: Boolean) = {
    abstract class PositionMovement(val px: Int, val py: Int) {
      def moveForward: PositionMovement

      def turnLeft: PositionMovement
      def turnRight: PositionMovement
      def reverse: PositionMovement

      def currentNode = (px, py)
    }

    case class Forward(x: Int, y: Int) extends PositionMovement(x, y) {
      override def moveForward: PositionMovement = Forward(x, y - 1)
      override def turnLeft: PositionMovement = Left(x, y)
      override def turnRight: PositionMovement = Right(x, y)
      override def reverse: PositionMovement = Down(x, y)
    }

    case class Right(x: Int, y: Int) extends PositionMovement(x, y) {
      override def moveForward: PositionMovement = Right(x + 1, y)
      override def turnLeft: PositionMovement = Forward(x, y)
      override def turnRight: PositionMovement = Down(x, y)
      override def reverse: PositionMovement = Left(x, y)
    }

    case class Left(x: Int, y: Int) extends PositionMovement(x, y) {
      override def moveForward: PositionMovement = Left(x - 1, y)
      override def turnLeft: PositionMovement = Down(x, y)
      override def turnRight: PositionMovement = Forward(x, y)
      override def reverse: PositionMovement = Right(x, y)
    }

    case class Down(x: Int, y: Int) extends PositionMovement(x, y) {
      override def moveForward: PositionMovement = Down(x, y + 1)
      override def turnLeft: PositionMovement = Right(x, y)
      override def turnRight: PositionMovement = Left(x, y)
      override def reverse: PositionMovement = Forward(x, y)
    }

    case class VirusSimple(nodes: Array[Array[Boolean]], affected: Int, pos: PositionMovement) extends Virus {

      override def getAffected() = affected
      override def burst() = {
        val (x, y) = pos.currentNode
        val nextMovement = if (nodes(y)(x)) pos.turnRight else pos.turnLeft
        VirusSimple(
          invertNode(x, y),
          if (nodes(y)(x)) affected + 1 else affected,
          nextMovement.moveForward
        )
      }

      def invertNode(x: Int, y: Int) = {
        nodes(y)(x) = !nodes(y)(x)
        nodes
      }
    }

    case class VirusEvolved(
                      nodes: Array[Array[Int]],
                      affected: Int,
                      pos: PositionMovement) extends Virus {

      // 0: Infected, 1: Weakened, 2: Flagged, 3: Clean
      override def getAffected() = affected

      override def burst() = {
        val (x, y) = pos.currentNode
        val (nextPos, nextAffected, nextNodes) = nodes(y)(x) match {
          case 0 => (pos.turnRight, affected, update(x, y, 2)) // Infected nodes become flagged.
          case 1 => (pos, affected + 1, update(x, y, 0))       // Weakened nodes become infected.
          case 2 => (pos.reverse, affected, update(x, y, 3))   // Flagged nodes become clean.
          case _ => (pos.turnLeft, affected, update(x, y, 1))  // Clean nodes become weakened.
        }

        VirusEvolved(nextNodes, nextAffected, nextPos.moveForward)
      }

      def update(x: Int, y: Int, v: Int) = {
        nodes(y)(x) = v
        nodes
      }
    }

    def burst(v: Virus, acc: Int): Virus = {
      if (acc == c) v
      else burst(v.burst(), acc + 1)
    }

    if (evolved) {
      val arr = Array.tabulate(450, 450) {
        case (y, x) => if (infected.contains((x - 200, y - 200))) 0 else 3
      }

      burst(VirusEvolved(arr, 0, Forward(212, 212)), 0)
    } else {
      val arr = Array.tabulate(200, 200) {
        case (y, x) => infected.contains((x - 50, y - 50))
      }

      burst(VirusSimple(arr, 0, Forward(62, 62)), 0)
    }
  }

}
