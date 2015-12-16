import scala.io.Source

object Day16 extends App {

  val regex = """Sue (\d+): (.+?): (\d+), (.+?): (\d+), (.+?): (\d+)""".r

  val data = Source
    .fromFile("inputs/input_day16.txt")
    .getLines
    .toList
    .map {
      case regex(id, n1, v1, n2, v2, n3, v3) => (id, Map(
        n1 -> v1.toInt,
        n2 -> v2.toInt,
        n3 -> v3.toInt)
        .withDefaultValue(0))
    }

  val expected = Map(
    "children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0,
    "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1)

  println(data
    .filter {
      case (_, map) => expected.keys.forall(k => (map(k) == expected(k)) || (map(k) == 0))
    }.filter {
      case (_, map) => map.values.forall(_ > 0)
    }.map(_._1)
    .head)

  println(data
    .filter {
      case (_, map) => expected.keys.forall(k => {
        k match {
          case ("cats" | "trees") => (map(k) > expected(k)) || (map(k) == 0)
          case ("pomeranians" | "goldfish") => (map(k) < expected(k)) || (map(k) == 0)
          case _ => (map(k) == expected(k)) || (map(k) == 0)
        }
      })
    }.filter {
    case (_, map) => map.values.forall(_ > 0)
  }.map(_._1)
  .head)
}