import scala.io.Source
import scala.util.Using

object Day23 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day23.txt")) {
      src => {
        val nums = src.getLines.mkString.toList.map(_ - '0')

      }
    }
  }
}
