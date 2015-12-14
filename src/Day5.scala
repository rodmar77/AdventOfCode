import scala.io.Source

object Day5 extends App {

  def isNice(s: String) = {
    def isDef(regex: String) = regex.r.findFirstIn(s).isDefined

    val threeVowels = isDef("""(.*[aeiou].*){3,}""")
    val doubleLetter = isDef(""".*(.)\1.*""")
    val notForbidden = List[String]("ab", "cd", "pq", "xy").forall(!s.contains(_))

    threeVowels && doubleLetter && notForbidden
  }

  def isNicer(s: String) = {
    def isDef(regex: String) = regex.r.findFirstIn(s).isDefined

    val twoDoubleLetter = isDef(""".*(..).*\1.*""")
    val repeatWithBetween = isDef(""".*(.).\1.*""")

    twoDoubleLetter && repeatWithBetween
  }

  val lines = Source.fromFile("inputs/input_day05.txt").getLines.toList
  println(lines.count(isNice))
  println(lines.count(isNicer))

}