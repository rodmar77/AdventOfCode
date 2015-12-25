import scala.io.Source

object Day13 extends App {

  val regex = """([^ ]+) .+? (gain|lose) (\d+) .+? to ([^.]+).""".r

  val data = Source
    .fromFile("inputs/input_day13.txt")
    .getLines
    .toList

  val happiness = data
    .map {
      case regex(first, status, amount, second) => (first, second) -> valueOf(status)(amount.toInt)
    }
    .toMap
    .withDefaultValue(0)

  println(getMax)
  println(getMax(true))

  def valueOf(s: String)(i: Int) = if (s == "gain") i else -i

  def getMax(implicit includeExtra: Boolean = false) = {
    val namesWithPermutations = {
      val names = data
        .map(l => l.substring(0, l.indexOf(' ')))
        .distinct

      (if (includeExtra) names :+ "Me" else names).permutations
    }

    def getArrangements(s: Seq[String]): Seq[(String, String)] = s
      .sliding(2)
      .flatMap { case (Seq(a, b)) => Seq((a, b), (b, a)) }
      .toList :+(s.last, s.head) :+(s.head, s.last)

    namesWithPermutations
      .map(getArrangements)
      .map(_.map(happiness).sum)
      .max
  }

}