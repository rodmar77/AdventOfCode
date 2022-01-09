import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Using
import scala.Nothing

object Day24 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day24.txt")) {
      source =>
        val program = source.getLines().toList
        println(run(program, 1))
        println(run(program, 2))
    }
  }

  def run(program: List[String], direction: Int): String = {
    val instruction = "([a-z]+) ([a-z]+) (.+)".r
    val range = if (direction == 1) (1 to 9).reverse else 1 to 9

    def run(block: List[String], input: Int, z: Int = 0): Map[String, Any] = {
      val functions = Map[String, (Int, Int) => Int](
        "add" -> (_ + _),
        "mul" -> (_ * _),
        "div" -> (_ / _),
        "mod" -> (_ % _),
        "eql" -> ((a, b) => if (a == b) 1 else 0)
      )

      @tailrec
      def run(block: List[String], variables: Map[String, Any]): Map[String, Any] = {
        def value(v: String) = if (v.forall(_.isLetter)) variables(v) else v.toInt
        def eval(op: String, a: String, b: String) = (variables(a), value(b)) match {
          case (x: Int, y: Int) => functions(op)(x, y)
          case (x: Any, y: Any) => op match {
            case "add" => if (x == 0) y else if (y == 0) x else None
            case "mul" => if (x == 0 || y == 0) 0 else if (x == 1) y else if (y == 1) x else None
            case "div" => if (x == 0) 0 else if (y == 1) x else None
            case "mod" => if (x == 0 || y == 1) 0 else None
            case "eql" => (a, y) match {
              case ("w", b: Int) if b < 1 || b > 9 => 0
              case _ => None
            }
          }
        }

        block match {
          case Nil => variables
          case x :: xs => x match {
            case instruction(op, a, b) => run(xs, variables + (a -> eval(op, a, b)))
          }
        }
      }

      run(block, Map("w" -> input, "x" -> 0, "y" -> 0, "z" -> z))
    }

    def zTarget(block: List[String]) = {
      def value(v: String) = if (v.forall(_.isLetter)) 1000 else v.toInt
      block.find {
        case instruction(a, b, c) => "add".equals(a) && "x".equals(b) && value(c) < 0
      }.map {
        case instruction("add", "x", c) => -value(c)
      }
    }

    @tailrec
    def extractBlocks(p: List[String], blocks: List[List[String]]): List[List[String]] = p.indexWhere(_.equals("inp w")) match {
      case i: Int if i < 0 => blocks :+ p
      case i: Int if i > 0 => p.splitAt(i) match {
        case (left, right) => extractBlocks(right.tail, blocks :+ left)
      }
    }

    val blocks = extractBlocks(program.tail, Nil)
    val results = range.foldLeft(ListMap[Int, List[Int]]()) {
      case (map, w) => run(blocks.head, w)("z") match {
        case z: Int => map + (z -> (map.getOrElse(z, Nil) :+ w))
      }
    }

    def evaluateBlock(previous: ListMap[Int, List[Int]], block: List[String]) = {
      val zt = zTarget(block)
      def evaluateBlockWithInput(current: ListMap[Int, List[Int]], input: Int) = {
        def evaluateBlockWithTarget(current: ListMap[Int, List[Int]], z: Int) = {
          if (zt.exists(x => (z % 26 - x) != input)) current
          else run(block, input, z)("z") match {
            case z1: Int =>
              if (current.contains(z1)) current
              else current + (z1 -> (previous(z) :+ input))
          }
        }

        previous.keys.foldLeft(current)(evaluateBlockWithTarget)
      }

      range.foldLeft(ListMap[Int, List[Int]]())(evaluateBlockWithInput)
    }

    blocks.tail.foldLeft(results)(evaluateBlock)(0).mkString
  }
}