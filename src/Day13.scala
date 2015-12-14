import scala.io.Source

object Day13 extends App {

  var regex = """([^ ]+) would (gain|lose) (\d+) happiness units by sitting next to ([^.]+).""".r

  val data = Source
    .fromFile("/tmp/input.txt")
    .getLines
    .toList

  val happiness = data
    .map(regex.findFirstMatchIn(_).get)
    .map(t => (t.group(1), t.group(4)) -> valueOf(t.group(2), t.group(3).toInt))
    .toMap

  println(
    data
      .map(l => l.substring(0, l.indexOf(' ')))
      .distinct
      .permutations
      .map(getArrangements)
      .map(_.map(happiness).sum)
      .max)

  def valueOf(s: String, i: Int) = if (s == "gain") i else -i

  def getArrangements(s: Seq[String]): Seq[(String, String)] = s
    .indices
    .init
    .flatMap(i => Seq[(String, String)](
      (s(i), s(i + 1)),
      (s(i + 1), s(i)))) :+(s.last, s.head) :+(s.head, s.last)

}
