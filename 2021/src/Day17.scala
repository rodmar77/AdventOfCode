import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17 {

  private val data = """target area: x=([-0-9]+)\.\.([-0-9]+), y=([-0-9]+)\.\.([-0-9]+)""".r

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day17.txt")) {
      source => source.getLines().mkString match {
        case data(a, b, c, d) =>
          val (x0, x1, y0, y1, height) = (
            a.toInt, b.toInt, c.toInt, d.toInt,
            (c.toInt max d.toInt) - (c.toInt min d.toInt))

          val ll = (1 to (x0 max x1)).flatMap(x =>  (-4*height to 4*height).map(maxHeight(x0, x1, y0, y1, x, _)))
          println(ll.max)
          println(ll.count(_ >= 0))
      }
    }
  }

  def maxHeight(x0: Int, x1: Int, y0: Int, y1: Int, x: Int, y: Int): Int = {
    @tailrec
    def maxHeight(currX: Int, xInc: Int, currY: Int, yInc: Int, maxY: Int): Int = {
      if ((currX > (x0 max x1)) || (currY < (y0 min y1))) -1
      else if (currX >= (x0 min x1) && currX <= (x0 max x1) && currY >= (y0 min y1) && currY <= (y0 max y1)) maxY
      else maxHeight(currX + xInc, (xInc - 1) max 0, currY + yInc, yInc - 1, currY max maxY)
    }

    maxHeight(0, x, 0, y, 0)
  }

}
