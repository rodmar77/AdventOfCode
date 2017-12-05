import scala.io.Source

object Day05 {

  def main(args: Array[String]): Unit = {
    val s = Source
      .fromFile("inputs/2017/input_day05.txt")
      .getLines
      .toArray
      .map(_.toInt)

    val s2 = s.clone()

    println(countJumpsToExit(s))
    println(countJumpsToExit2(s2))
  }

  def countJumpsToExit(ints: Array[Int]) = {
    def countJumpsToExit(ll: Array[Int], idx: Int, acc: Int): Int = {
      if (idx < 0 || idx >= ll.length) acc
      else {
        val nextIndex = idx + ll(idx)
        ll(idx) = ll(idx) + 1
        countJumpsToExit(ll, nextIndex, acc + 1)
      }
    }

    countJumpsToExit(ints, 0, 0)
  }

  def countJumpsToExit2(ints: Array[Int]) = {
    def countJumpsToExit(ll: Array[Int], idx: Int, acc: Int): Int = {
      if (idx < 0 || idx >= ll.length) acc
      else {
        val nextIndex = idx + ll(idx)
        ll(idx) = if (ll(idx) >= 3) ll(idx) - 1 else ll(idx) + 1
        countJumpsToExit(ll, nextIndex, acc + 1)
      }
    }

    countJumpsToExit(ints, 0, 0)
  }
}
