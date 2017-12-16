import scala.io.Source

object Day19 extends App {
  implicit def stringToString(s: String) = new StringWithReplaceAt(s)

  class StringWithReplaceAt(value: String) {
    def cutAndAdd(data: String, pos: Int, size: Int) = value.substring(0, pos) + data + value.substring(pos + size)
    def countStr(s: String) = {
      def count(i: Int, acc: Int): Int = {
        val nextIndex = value.indexOf(s, i + 1)
        if (nextIndex < 0) acc
        else count(nextIndex + s.length - 1, acc + 1)
      }

      count(-1, 0)
    }
  }

  val regex = "(.+?) => (.+?)".r
  val data = Source
    .fromFile("inputs/2015/input_day19.txt")
    .getLines
    .toList

  val replacements = data.takeWhile(!_.isEmpty).map { case regex(from, to) => (from, to) }
  println(replacements.flatMap(getAllReplacements(data.last, _)).distinct.size)
  println(getMinReplacementCount(data.last))

  def getAllReplacements(s: String, r: (String, String)): List[String] = {
    def getAllReplacements(curr: Int, acc: List[String]): List[String] = {
      if (curr < 0) acc
      else getAllReplacements(
        s.indexOf(r._1, curr + 1),
        acc :+ s.cutAndAdd(r._2, curr, r._1.length))
    }

    getAllReplacements(s.indexOf(r._1), Nil)
  }

  /*

  All of the rules are of one of the following forms:

  -> x => 1|2
  -> x => 1|Rn|2|Ar
  -> x => 1|Rn|2|Y|3|Ar
  -> x => 1|Rn|2|Y|3|Y|4|Ar

  As Rn, Ar, and Y are only on the left side of the equation, one merely only needs to compute

  #NumSymbols - #Rn - #Ar - 2 * #Y - 1
  Subtract of #Rn and #Ar because those are just extras. Subtract two times #Y because we get rid of
  the Ys and the extra elements following them. Subtract one because we start with "e".

  */
  def getMinReplacementCount(str: String) = {
    str.count(_.isUpper) - str.countStr("Rn") - str.countStr("Ar") - 2 * str.countStr("Y") - 1
  }

}
