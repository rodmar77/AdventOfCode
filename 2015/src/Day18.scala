import scala.io.Source

object Day18 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day18.txt")
    .getLines
    .map(_.toCharArray.toList)
    .toList

  println(life(100))
  println(life(100, (0, 0), (0, data.size-1), (data.size-1, 0), (data.size-1, data.size-1)))

  def life(times: Int, stuck: (Int, Int)*) = {
    def getNeighborCount(i: Int, j: Int, acc: List[List[Char]]) = {
      (-1 to 1).map(x => (-1 to 1).map(y =>
        if (((x == 0) && (y == 0)) ||
          (!acc.isDefinedAt(i + x)) ||
          (!acc(i + x).isDefinedAt(j + y)) ||
          (acc(i + x)(j + y) == '.')) 0
        else 1)
        .sum)
        .sum
    }

    def life(curr: Int, acc: List[List[Char]]): List[List[Char]] = {
      if (curr == times) acc
      else life(curr + 1, List.tabulate(acc.size, acc.head.size)((i, j) => {
        val n = getNeighborCount(i, j, acc)
        if (stuck.contains((i, j))) '#'
        else if (((acc(i)(j) == '#') && ((n == 2) || (n == 3))) ||
          ((acc(i)(j) == '.') && (n == 3))) '#'
        else '.'
      }))
    }

    life(0, data).map(_.count(_ == '#')).sum
  }
}
