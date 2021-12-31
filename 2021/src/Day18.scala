import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18 {

  case class Number(value: Int, level: Int)

  def parse(s: String): List[Number] = {
    @tailrec
    def parse(xs: List[Char], level: Int, acc: List[Number]): List[Number] = xs match {
      case Nil => acc
      case h :: t if h.isDigit => parse(t, level, acc :+ Number(h.asDigit, level))
      case '[' :: t => parse(t, level + 1, acc)
      case ']' :: t => parse(t, level - 1, acc)
      case _ :: t => parse(t, level, acc)
    }

    parse(s.toList, -1, Nil)
  }

  def split(xs: List[Number], i: Int): List[Number] = {
    val (level, result) = (xs(i).level, xs(i).value / 2d)
    (result.floor.toInt, result.ceil.toInt) match {
      case (n1, n2) => xs.take(i) ++ List(Number(n1, level + 1), Number(n2, level + 1)) ++ xs.drop(i + 1)
    }
  }

  def explode(xs: List[Number], i: Int): List[Number] = i match {
      case 0 => xs.slice(1, 3) match {
        case List(n2, n3) => List(Number(0, 3), Number(n2.value + n3.value, n3.level)) ++ xs.drop(3)
      }
      case x if x < xs.length - 2 => xs.slice(i - 1, i + 3) match {
        case List(n0, n1, n2, n3) =>
          val mid = List(Number(n0.value + n1.value, n0.level), Number(0, 3), Number(n2.value + n3.value, n3.level))
          xs.take(x - 1) ++ mid ++ xs.drop(x + 3)
      }
      case _ => xs.takeRight(3) match {
        case List(n1, n2, _) => xs.take(i - 1) ++ List(Number(n1.value + n2.value, 3), Number(0, 3))
      }
    }

  @tailrec
  def magnitude(xs: List[Number]): Int = xs match {
    case h :: Nil => h.value
    case _ =>
      val maxLevel = xs.map(_.level).max
      xs.span(_.level != maxLevel) match {
        case (head, n1 +: n2 +: ns) => magnitude((head :+ Number(3 * n1.value + 2 * n2.value, maxLevel - 1)) ++ ns)
      }
  }

  def join(a: List[Number], b: List[Number]): List[Number] = reduce(
    (a ++ b).map(num => Number(num.value, num.level + 1))
  )

  def reduce(xs: List[Number]): List[Number] = Iterator
    .iterate((xs, false)) {
      case (xs, _) => xs
        .zipWithIndex
        .find {
          case (n, _) => n.level == 4
        } match {
            case Some((_, i)) => (explode(xs, i), false)
            case None => xs.zipWithIndex.find {
                case (n, _) => n.value > 9
              } match {
                case Some((_, i)) => (split(xs, i), false)
                case None => (xs, true)
              }
        }
    }
    .dropWhile {
      case (_, v) => !v
    }.next match {
      case (list, _) => list
    }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day18.txt")) {
      source =>
        val input = source.getLines().map(parse).toList
        println(magnitude(input.tail.foldLeft(input.head)(join)))
        println(input
          .combinations(2)
          .flatMap(
            _.permutations.map(p =>
              magnitude(join(p.head, p.last))))
          .max)
    }
  }

}
