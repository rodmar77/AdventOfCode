import Day04.dataExtractor

import scala.io.Source

object Day04 {

  val idExtractor = ".+?-([0-9]+)\\[.+\\]".r
  val dataExtractor = "(.+?)-[0-9]+\\[(.+)\\]".r

  def main(args: Array[String]): Unit = {
    val ids = Source
      .fromFile("inputs/2016/input_day04.txt")
      .getLines
      .toList

    println(ids.filter(isValidRoom).map(extractId).sum)
    println(ids.map(s => (rotate(s), extractId(s))).find(p => p._1.startsWith("NORTHPOLE")).get._2)
  }

  def extractId(s: String) = idExtractor.findFirstMatchIn(s).get.group(1).toInt

  def isValidRoom(s: String): Boolean = {
    def expected(t: String) = t
      .replace("-", "")
      .groupBy(_.toChar)
      .map(p => (p._1, p._2.length))
      .toList
      .sortWith((a, b) => (a._2 > b._2) || ((a._2 == b._2) && (a._1 < b._1)))
      .map(_._1)
      .mkString
      .take(5)

    val dataExtractor(letters, actual) = s
    expected(letters).equals(actual)
  }

  def rotate(s: String): String = {
    val (alphabet, dataExtractor(letters, _), idExtractor(id)) = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", s, s)
    letters.map(c => {
      if (c == '-') '-'
      else {
        val (idx, shift) = (alphabet.indexOf(c.toUpper), id.toInt)
        alphabet((idx + shift) % alphabet.size)
      }
    })
  }
}
