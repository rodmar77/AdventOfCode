import scala.io.Source

object Day18 {

  def main(args: Array[String]): Unit = {
    val start = Source
      .fromFile("inputs/2016/input_day18.txt")
      .getLines
      .toList
      .head

    println(getSafeTileCount(start, 40)._2)
    println(getSafeTileCount(start, 400000)._2)
  }

  def getSafeTileCount(start: String, count: Int): (String, Int) = {
    def safeTileCount(row: String) = row.count(_ == '.')

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

    (0 until count - 1).foldLeft((start, safeTileCount(start)))((a, _) => nextValueFor(a))
  }
}
