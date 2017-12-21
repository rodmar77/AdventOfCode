import scala.io.Source

object Day18 {

  def main(args: Array[String]): Unit = {
    val start = Source
      .fromFile("inputs/2016/input_day18.txt")
      .getLines
      .toList
      .head

    println(getSafeTileCount(start, 40))
    println(getSafeTileCount(start, 400000))
  }

  def getSafeTileCount(start: String, count: Int): Int = {
    def safeTileCount(row: String) = row.count(_ == '.')

    def _getSafeTileCount(idx: Int, rowData: (String, Int)): Int = {
      def nextValueFor(rowData: (String, Int)): (String, Int) = {
        def getNextRow(str: String): String = {
          def ch(i: Int) = if (str.isDefinedAt(i)) str(i) else '.'
          def trap(i: Int) = ch(i) == '^'

          str.indices.map(idx => {
            val (l, r) = (trap(idx - 1), trap(idx + 1))
            if ((l && !r) || (!l && r))
              '^'
            else
              '.'
          }).mkString
        }

        val nextRow = getNextRow(rowData._1)
        (nextRow, safeTileCount(nextRow) + rowData._2)
      }

      if (idx == count) rowData._2
      else _getSafeTileCount(idx + 1, nextValueFor(rowData))
    }

    _getSafeTileCount(1, (start, safeTileCount(start)))
  }
}
