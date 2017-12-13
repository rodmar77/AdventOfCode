import scala.io.Source

object Day18 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day18.txt")
    .getLines
    .map(_.toCharArray.toList)
    .toList

  println(life(data, 100))
  println(life(data, 100, (0, 0), (0, data.size-1), (data.size-1, 0), (data.size-1, data.size-1)))

  def life(data: Seq[Seq[Char]], times: Int, stuck: (Int, Int)*) = {
    def life(curr: Int, acc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      def isLive(x: Int, y: Int) = stuck.contains((x, y)) || (acc.isDefinedAt(y) && acc(y).isDefinedAt(x) && acc(y)(x) == '#')

      def getNeighborCount(i: Int, j: Int) = {
        val areaTotal = (-1 to 1).map(y => (-1 to 1).count(x => isLive(x + i, y + j))).sum
        if (isLive(i, j)) areaTotal - 1 else areaTotal
      }

      def getValueForLiveCell(x: Int, y: Int): Char = {
        def staysLive(nc: Int) = stuck.contains((x, y)) || nc == 2 || nc == 3
        if (staysLive(getNeighborCount(x, y))) '#' else '.'
      }

      def getValueForDeadCell(x: Int, y: Int): Char = {
        def staysDead(nc: Int) = nc != 3
        if (staysDead(getNeighborCount(x, y))) '.' else '#'
      }

      if (curr == times) acc
      else life(
        curr + 1,
        acc.indices.map(y => acc(y).indices.map(x => {
          if (isLive(x, y)) getValueForLiveCell(x, y)
          else getValueForDeadCell(x, y)
        })))
    }

    life(0, data).map(_.count(_ == '#')).sum
  }
}
