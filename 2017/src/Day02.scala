import scala.io.Source

object Day02 {

  def main(args: Array[String]): Unit = {
    val s = Source
      .fromFile("inputs/2017/input_day02.txt")
      .getLines
      .map(n => n.split("\\s+").map(_.toInt))
      .toList

    println(s
      .map(n => n.max - n.min)
      .sum)

    println(s
      .map(
        arr => arr.map(
          x => (x, arr.find(
            y => (y > x) && (y % x == 0)))))
      .flatMap(arr => arr.filterNot(_._2.isEmpty))
      .map(p => p._2.get / p._1)
      .sum)

  }

}
