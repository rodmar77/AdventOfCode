import scala.io.Source

object Day19 extends App {
  implicit def stringToString(s: String) = new StringWithReplaceAt(s)

  class StringWithReplaceAt(value: String) {
    def cutAndAdd(data: String, pos: Int, size: Int) = value.substring(0, pos) + data + value.substring(pos + size)
  }

  val regex = "(.+?) => (.+?)".r

  val data = Source
    .fromFile("inputs/2015/input_day19.txt")
    .getLines
    .toList

  val replacements = data.init.map { case regex(from, to) => (from, to) }
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

  def getMinReplacementCount(s: String) = {
    val mem = collection.mutable.Map[String, Int]()

    def getAllIndexesFor(curr: String, r: String) = {
      def getAllIndexesFor(i: Int, acc: List[Int]): List[Int] = {
        if (i < 0) acc
        else getAllIndexesFor(curr.indexOf(r, i + 1), acc :+ i)
      }

      getAllIndexesFor(curr.indexOf(r), Nil)
    }

    def getMinReplacementCount(curr: String, acc: Int): Int = {
      mem.values.find(_ != Int.MaxValue).getOrElse(mem.getOrElseUpdate(curr, {
        if (replacements.exists(_._1 == curr)) acc
        else if (!replacements.exists(p => curr.contains(p._2))) Int.MaxValue
        else
          replacements
            .flatMap(p => getAllIndexesFor(curr, p._2)
              .map(curr.cutAndAdd(p._1, _, p._2.length)))
            .distinct
            .map(getMinReplacementCount(_, acc + 1))
            .min
      }))
    }

    getMinReplacementCount(s, 0)
  }

}
