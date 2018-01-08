import scala.io.Source

/*

  --- Day 20: Firewall Rules ---

  You'd like to set up a small hidden computer here so you can use it to get
  back into the network later. However, the corporate firewall only allows
  communication with certain external IP addresses.

  You've retrieved the list of blocked IPs from the firewall, but the list
  seems to be messy and poorly maintained, and it's not clear which IPs are
  allowed. Also, rather than being written in dot-decimal notation, they are
  written as plain 32-bit integers, which can have any value from 0 through
  4294967295, inclusive.

  For example, suppose only the values 0 through 9 were valid, and that you
  retrieved the following blacklist:

  5-8
  0-2
  4-7

  The blacklist specifies ranges of IPs (inclusive of both the start and end
  value) that are not allowed. Then, the only IPs that this firewall allows
  are 3 and 9, since those are the only numbers not in any range.

 */
object Day20 {

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromFile("inputs/2016/input_day20.txt")
      .getLines
      .toList
      .map(_.split("-"))
      .map(arr => (arr.head.toLong, arr.last.toLong))
      .sorted
      .foldLeft(List[(Long, Long)]()) { (acc, t) =>
      acc match {
        case x :: xs => if (x._2 >= t._1) (x._1, x._2.max(t._2)) :: xs else t +: acc
        case _ => List(t)
      }
    }.reverse

    // Given the list of blocked IPs you retrieved from the firewall (your puzzle
    // input), what is the lowest-valued IP that is not blocked?
    println(lines.sliding(2).map{
      case Seq((_, firstEnd), (secondStart, _)) => (firstEnd, secondStart)
    }.find {
      case (start, end) => end - start > 1
    } match {
      case Some((start, _)) => start + 1
    })

    // How many IPs are allowed by the blacklist?
    println(lines.sliding(2).map{
      case Seq((_, firstEnd), (secondStart, _)) => secondStart - firstEnd - 1
    }.sum)
  }

}
