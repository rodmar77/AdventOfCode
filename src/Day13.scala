import scala.io.Source
object Day13 extends App {

  val regex = """([^ ]+) would (gain|lose) (\d+) happiness units by sitting next to ([^.]+).""".r

  val data = Source
    .fromFile("inputs/input_day13.txt")
    .getLines
    .toList

  val happiness = data
    .map {
      case regex(first, status, amount, second) => (first, second) -> valueOf(status, amount.toInt)
    }
    .toMap
    .withDefaultValue(0)

  println(
    namesWithPermutations(false)
      .map(getArrangements)
      .map(_.map(happiness).sum)
      .max)

  println(
    namesWithPermutations(true)
      .map(getArrangements)
      .map(_.map(happiness).sum)
      .max)

  def valueOf(s: String, i: Int) = if (s == "gain") i else -i

  def namesWithPermutations(includeExtra: Boolean) = {
    val names = data
      .map(l => l.substring(0, l.indexOf(' ')))
      .distinct

    (if (includeExtra) names :+ "Me" else names).permutations
  }

  def getArrangements(s: Seq[String]): Seq[(String, String)] = s
    .sliding(2)
    .flatMap { case (Seq(a, b)) => Seq((a, b), (b, a))}
    .toList :+(s.last, s.head) :+(s.head, s.last)

}