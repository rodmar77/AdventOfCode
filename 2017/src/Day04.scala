import scala.io.Source

object Day04 {

  def main(args: Array[String]): Unit = {
    val s = Source
      .fromFile("inputs/2017/input_day04.txt")
      .getLines
      .toList

    println(s.count(isValidPassphrase))
    println(s.count(isValidPassphraseAnagrams))
  }

  def isValidPassphrase(p: String): Boolean = {
    val ps = p.split("\\s+")
    ps.distinct.length == ps.length
  }

  def isValidPassphraseAnagrams(p: String): Boolean = {
    val ps = p.split("\\s+").map(_.sorted)
    ps.distinct.length == ps.length
  }
}
