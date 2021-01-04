import scala.io.Source
import scala.util.Using

object Day17 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day17.txt")) {
      src => {
        val input = src.getLines.zipWithIndex.flatMap {
          case (line, y) => line.iterator.zipWithIndex.map {
            case (char, x) => (x, y) -> char
          }
        }.toMap

        println(Iterator.iterate(parseInput(3, input))(conway).drop(6).next().size)
        println(Iterator.iterate(parseInput(4, input))(conway).drop(6).next().size)
      }
    }
  }

  private def parseInput(dimensions: Int, input: Map[(Int, Int), Char]): Set[List[Int]] = {
    val fill = List.fill(dimensions - 2)(0)
    input.toList.filter {
      case (_, n) => n == '#'
    }.map {
      case (n, _) => n
    }.map {
      case (x, y) => List(x, y) ++ fill
    }.toSet
  }

  private def conway(cells: Set[List[Int]]): Set[List[Int]] = cells.flatMap(neighbors).flatMap(cell => {
      val activeNeighbors = neighbors(cell).count(neighbor => cells.contains(neighbor) && neighbor != cell)
      if (activeNeighbors == 3 || (activeNeighbors == 2 && cells.contains(cell))) Set(cell)
      else Set.empty
    })

  private def neighbors(cell: List[Int]): Iterator[List[Int]] =
    if (cell.isEmpty) Iterator(Nil)
    else {
      val head = cell.head
      neighbors(cell.tail).flatMap(tail => Iterator((head - 1) :: tail, head :: tail, (head + 1) :: tail))
    }
}
