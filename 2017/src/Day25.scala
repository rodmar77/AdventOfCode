import scala.io.Source

/*

  --- Day 25: The Halting Problem ---

  Following the twisty passageways deeper and deeper into the CPU, you finally
  reach the core of the computer. Here, in the expansive central chamber, you
  find a grand apparatus that fills the entire room, suspended nanometers above
  your head.

  You had always imagined CPUs to be noisy, chaotic places, bustling with activity.
  Instead, the room is quiet, motionless, and dark.

  Suddenly, you and the CPU's garbage collector startle each other. "It's not
  often we get many visitors here!", he says. You inquire about the stopped machinery.

  "It stopped milliseconds ago; not sure why. I'm a garbage collector, not a
  doctor." You ask what the machine is for.

  "Programs these days, don't know their origins. That's the Turing machine! It's
  what makes the whole computer work." You try to explain that Turing machines
  are merely models of computation, but he cuts you off. "No, see, that's just
  what they want you to think. Ultimately, inside every CPU, there's a Turing
  machine driving the whole thing! Too bad this one's broken. We're doomed!"

  You ask how you can help. "Well, unfortunately, the only way to get the
  computer running again would be to create a whole new Turing machine from
  scratch, but there's no way you can-" He notices the look on your face, gives
  you a curious glance, shrugs, and goes back to sweeping the floor.

  You find the Turing machine blueprints (your puzzle input) on a tablet in a
  nearby pile of debris. Looking back up at the broken Turing machine above,
  you can start to identify its parts:

  - A tape which contains 0 repeated infinitely to the left and right.
  - A cursor, which can move left or right along the tape and read or write
    values at its current position.
  - A set of states, each containing rules about what to do based on the current
    value under the cursor.
  - Each slot on the tape has two possible values: 0 (the starting value for all
    slots) and 1. Based on whether the cursor is pointing at a 0 or a 1, the current
    state says what value to write at the current position of the cursor, whether
    to move the cursor left or right one slot, and which state to use next.

  For example, suppose you found the following blueprint:

  Begin in state A.
  Perform a diagnostic checksum after 6 steps.

  +-----------------------------------+
  | In state A:                       |
  |   If the current value is 0:      |
  |     - Write the value 1.          |
  |     - Move one slot to the right. |
  |     - Continue with state B.      |
  |   If the current value is 1:      |
  |     - Write the value 0.          |
  |     - Move one slot to the left.  |
  |     - Continue with state B.      |
  |                                   |
  | In state B:                       |
  |   If the current value is 0:      |
  |     - Write the value 1.          |
  |     - Move one slot to the left.  |
  |     - Continue with state A.      |
  |   If the current value is 1:      |
  |     - Write the value 1.          |
  |     - Move one slot to the right. |
  |     - Continue with state A.      |
  +-----------------------------------+

  Running it until the number of steps required to take the listed diagnostic
  checksum would result in the following tape configurations (with the cursor
  marked in square brackets):

  +-------------------------------------------------------------------+
  | ... 0  0  0 [0] 0  0 ... (before any steps; about to run state A) |
  | ... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B) |
  | ... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A) |
  | ... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B) |
  | ... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A) |
  | ... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B) |
  | ... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A) |
  +-------------------------------------------------------------------+

  The CPU can confirm that the Turing machine is working by taking a diagnostic
  checksum after a specific number of steps (given in the blueprint). Once the
  specified number of steps have been executed, the Turing machine should pause;
  once it does, count the number of times 1 appears on the tape. In the above
  example, the diagnostic checksum is 3.

 */
object Day25 {

  def main(args: Array[String]): Unit = {
    val groups = splitGroups(
      Source
      .fromFile("inputs/2017/input_day25.txt")
      .getLines
      .toList)

    val states = toStates(groups.tail)

    // Recreate the Turing machine and save the computer! What is the diagnostic
    // checksum it produces once it's working again?
    val tape = runMachine(states, groups.head)
    println(tape.values.sum)
  }

  def runMachine(states: List[State], data: List[String]) = {
    def runMachine(state: State, tape: Map[Int, Int], cursor: Int, count: Long, total: Long): Map[Int, Int] = {
      if (count == total) tape
      else state.execute(states, tape, cursor) match {
        case (nextTape, nextCursor, nextState) => runMachine(nextState, nextTape, nextCursor, count + 1, total)
      }
    }

    val (startState, iterations) = (
      "Begin in state (\\w+).".r,
      "Perform a diagnostic checksum after (\\d+) steps.".r
    )

    val (startState(name), iterations(count)) = (data.head, data.last)
    states.find(s => s.name.equals(name)) match {
      case Some(state) => runMachine(state, Map().withDefaultValue(0), 0, 0, count.toLong)
    }
  }

  abstract class Step {
    def execute(states: List[State], state: State, tape: Map[Int, Int], cursor: Int): (Map[Int, Int], Int, State)
  }

  case class Write(v: Int) extends Step {
    override def execute(states: List[State], state: State, tape: Map[Int, Int], cursor: Int) = (tape + (cursor -> v), cursor, state)
  }

  case class Move(direction: String) extends Step {
    override def execute(states: List[State], state: State, tape: Map[Int, Int], cursor: Int) = direction match {
      case "left" => (tape, cursor - 1, state)
      case _ => (tape, cursor + 1, state)
    }
  }

  case class Next(sn: String) extends Step {
    override def execute(states: List[State], state: State, tape: Map[Int, Int], cursor: Int) = {
      states.find(st => sn.equals(st.name)) match {
        case Some(nextState) => (tape, cursor, nextState)
      }
    }
  }

  case class State(name: String, steps: Map[Int, List[Step]]) {
    def execute(states: List[State], tape: Map[Int, Int], cursor: Int): (Map[Int, Int], Int, State) = {
      def execute(steps: List[Step], tape: Map[Int, Int], cursor: Int, state: State): (Map[Int, Int], Int, State) = {
        if (steps.isEmpty) (tape, cursor, state)
        else steps.head.execute(states, state, tape, cursor) match {
          case (nextTape, nextCursor, nextState) => execute(steps.tail, nextTape, nextCursor, nextState)
        }
      }

      execute(steps(tape(cursor)), tape, cursor, this)
    }
  }

  def toStates(groups: List[List[String]]) = {
    val (groupHeader, stepsHeader) = ("In state (\\w+):".r, "\\s*If the current value is (0|1):".r)

    def extractSteps(stepGroups: List[String]): Map[Int, List[Step]] = {
      def extractStepGroup(stepGroup: List[String], key: Int, acc: Map[Int, List[Step]]): Map[Int, List[Step]] = {
        val (write, move, next) = (
          "\\s*- Write the value (\\d).".r,
          "\\s*- Move one slot to the (left|right).".r,
          "\\s*- Continue with state (\\w+).".r
        )

        acc + (key ->
        stepGroup.map {
          case write(digit) => Write(digit.toInt)
          case move(direction) => Move(direction)
          case next(state) => Next(state)
        })
      }

      def extractSteps(stepGroups: List[String], acc: Map[Int, List[Step]]): Map[Int, List[Step]] = {
        if (stepGroups.isEmpty) acc
        else stepGroups.head match {
          case stepsHeader(digit) => extractSteps(
            stepGroups
              .tail
              .dropWhile(
                !_.matches(stepsHeader.regex)),
            extractStepGroup(stepGroups
              .tail
              .takeWhile(
                !_.matches(stepsHeader.regex)),
              digit.toInt,
              acc))
        }
      }

      extractSteps(stepGroups, Map())
    }

    def toState(group: List[String]) = group.head match {
      case groupHeader(name) => State(name, extractSteps(group.tail))
    }

    groups.map(toState)
  }

  def splitGroups(lines: List[String]) = {
    def splitGroups(ll: List[String], acc: List[List[String]]): List[List[String]] = {
      if (ll.isEmpty) acc
      else if (ll.head.isEmpty) splitGroups(ll.tail, acc)
      else splitGroups(
        ll.dropWhile(!_.isEmpty),
        acc :+ ll.takeWhile(!_.isEmpty))
    }

    splitGroups(lines, List())
  }
}