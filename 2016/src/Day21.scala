import scala.io.Source

/*

  --- Day 21: Scrambled Letters and Hash ---

  The computer system you're breaking into uses a weird scrambling function to
  store its passwords. It shouldn't be much trouble to create your own scrambled
  password so you can add it to the system; you just have to implement the
  scrambler.

  The scrambling function is a series of operations (the exact list is provided
  in your puzzle input). Starting with the password to be scrambled, apply each
  operation in succession to the string. The individual operations behave as
  follows:

  [swap position X with position Y] means that the letters at indexes X and Y
  (counting from 0) should be swapped.

  [swap letter X with letter Y] means that the letters X and Y should be swapped
  (regardless of where they appear in the string).

  [rotate left/right X steps] means that the whole string should be rotated; for
  example, one right rotation would turn abcd into dabc.

  [rotate based on position of letter X] means that the whole string should be
  rotated to the right based on the index of letter X (counting from 0) as
  determined before this instruction does any rotations. Once the index is
  determined, rotate the string to the right one time, plus a number of times
  equal to that index, plus one additional time if the index was at least 4.

  [reverse positions X through Y] means that the span of letters at indexes X
  through Y (including the letters at X and Y) should be reversed in order.

  [move position X to position Y] means that the letter which is at index X
  should be removed from the string, then inserted such that it ends up at
  index Y.

  For example, suppose you start with abcde and perform the following operations:

  - [swap position 4 with position 0] swaps the first and last letters, producing
    the input for the next step, ebcda.

  - [swap letter d with letter b] swaps the positions of d and b: edcba.

  - [reverse positions 0 through 4] causes the entire string to be reversed,
    producing abcde.

  - [rotate left 1 step] shifts all letters left one position, causing the first
    letter to wrap to the end of the string: bcdea.

  - [move position 1 to position 4] removes the letter at position 1 (c), then
    inserts it at position 4 (the end of the string): bdeac.

  - [move position 3 to position 0] removes the letter at position 3 (a), then
    inserts it at position 0 (the front of the string): abdec.

  - [rotate based on position of letter b] finds the index of letter b (1), then
    rotates the string right once plus a number of times equal to that index (2):
    ecabd.

  - [rotate based on position of letter d] finds the index of letter d (4), then
    rotates the string right once, plus a number of times equal to that index,
    plus an additional time because the index was at least 4, for a total of 6
    right rotations: decab.

  After these steps, the resulting scrambled password is decab.

 */
object Day21 {

  def main(args: Array[String]): Unit = {
    val commands = Commands(
      Source
        .fromFile("inputs/2016/input_day21.txt")
        .getLines
        .toList)

    // Now, you just need to generate a new scrambled password and you can
    // access the system. Given the list of scrambling operations in your
    // puzzle input, what is the result of scrambling abcdefgh?
    println(commands.apply("abcdefgh"))

    // You scrambled the password correctly, but you discover that you can't
    // actually modify the password file on the system. You'll need to
    // un-scramble one of the existing passwords by reversing the scrambling
    // process.
    //
    // What is the un-scrambled version of the scrambled password fbgdceah?
    println(commands.unapply("fbgdceah"))
  }

  case class Commands(commands: List[String]) {
    private val (swapPositions, swapLetters, rotate, rotateBased, reverse, move) = (
      "swap position (\\d) with position (\\d)".r,
      "swap letter ([a-z]) with letter ([a-z])".r,
      "rotate (left|right) (\\d+) step[s]?".r,
      "rotate based on position of letter ([a-z])".r,
      "reverse positions (\\d) through (\\d)".r,
      "move position (\\d) to position (\\d)".r
    )

    private def doRotate(text: String, direction: String, steps: Int) = direction match {
      case "right" => text.drop(text.length - steps) + text.take(text.length - steps)
      case _ => text.drop(steps) + text.take(steps)
    }

    private def undoRotate(text: String, direction: String, steps: Int) = direction match {
      case "left" => doRotate(text, "right", steps)
      case _ => doRotate(text, "left", steps)
    }

    private def doRotateBased(text: String, letter: Char) = {
      val index = text.indexOf(letter)
      doRotate(text, "right", (1 + index + (if (index >= 4) 1 else 0)) % text.length)
    }

    private def undoRotateBased(text: String, letter: Char) = {
      val (count, direction) = text.indexOf(letter) match {
        case 0|1 => (1, "left")
        case 2 => (2, "right")
        case 3 => (2, "left")
        case 4 => (1, "right")
        case 5 => (3, "left")
        case 6 => (0, "right")
        case _ => (4, "left")
      }

      doRotate(text, direction, count)
    }

    private def doMove(text: String, from: Int, to: Int) = if (to.toInt > from.toInt)
      text.substring(0, from) +
        text.substring(from + 1, to + 1) +
        text.charAt(from) +
        text.substring(to + 1)
    else
      text.substring(0, to) +
        text.charAt(from) +
        text.substring(to, from) +
        text.substring(from + 1)

    private def doReverse(text: String, from: Int, to: Int) = text.substring(0, from) +
      text.substring(from, to + 1).reverse +
      text.substring(to + 1)


    def apply(text: String) = commands.foldLeft(text)((text, command) => command match {
      case swapPositions(a, b) => text.updated(a.toInt, text.charAt(b.toInt)).updated(b.toInt, text.charAt(a.toInt))
      case swapLetters(a, b) => text.updated(text.indexOf(a), b.head).updated(text.indexOf(b), a.head)
      case rotate(direction, steps) => doRotate(text, direction, steps.toInt % text.length)
      case rotateBased(letter) => doRotateBased(text, letter.head)
      case reverse(from, to) => doReverse(text, from.toInt, to.toInt)
      case move(from, to) => doMove(text, from.toInt, to.toInt)
    })

    def unapply(text: String) = commands.foldRight(text)((command, text) => command match {
      case swapPositions(a, b) => text.updated(a.toInt, text.charAt(b.toInt)).updated(b.toInt, text.charAt(a.toInt))
      case swapLetters(a, b) => text.updated(text.indexOf(a), b.head).updated(text.indexOf(b), a.head)
      case rotate(direction, steps) => undoRotate(text, direction, steps.toInt % text.length)
      case rotateBased(letter) => undoRotateBased(text, letter.head)
      case reverse(from, to) => doReverse(text, from.toInt, to.toInt)
      case move(from, to) => doMove(text, to.toInt, from.toInt)
    })
  }
}