import scala.io.Source

object Day21 {

  def main(args: Array[String]): Unit = {
    val commands = Commands(Source.fromFile("inputs/2016/input_day21.txt").getLines.toList)
    println(commands.apply("abcdefgh"))
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
        case 0 => (1, "left")
        case 1 => (1, "left")
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