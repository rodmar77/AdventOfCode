import scala.io.Source
import scala.util.Using

object Day14 {

    private val mapRegex = "([A-Z]+) -> ([A-Z])".r

    def main(args: Array[String]): Unit = {
      Using(Source.fromFile("inputs/2021/input_day14.txt")) {
        source =>
          val lines = source.getLines()
          val (template, rules) = (
            lines.next(),
            lines.dropWhile(_.isEmpty).map {
            case mapRegex(f, t) => f -> t
          }.toMap)

          val frequency10 = applyRules(template, rules, 10).values
          println(frequency10.max - frequency10.min)

          val frequency40 = applyRules(template, rules, 40).values
          println(frequency40.max - frequency40.min)
      }
    }

  def applyRules(template: String, rules: Map[String, String], days: Int) = {
    def applyRule(v: (String, Long)) = v match {
      case (k, v) => rules.get(k) match {
        case Some(rule) => List((k.head + rule) -> v, (rule + k.last) -> v)
        case None => List(k -> v)
      }
    }

    def key[A,B](v: (A, B)) = v match {
      case (k, _) => k
    }

    def value[A,B](v: (A, B)) = v match {
      case (_, v) => v
    }

    val initialCount = template.toSeq.sliding(2).toList.map(_.unwrap).groupBy(n => n).map {
      case (k, v) => k -> v.size.toLong
    }

    val result = (1 to days)
      .foldRight(initialCount) {
        case (_, count) =>
          count
            .map(applyRule)
            .flatten
            .groupBy(key)
            .map {
              case (k, v) => k -> v.map(value).sum
            }
      }.map {
        case (k, v) => List((k.head, v), (k.last, v))
      }.flatten
       .groupBy(key)
       .map {
         case (k, v) => (k, (v.map(value).sum + 1) / 2)
      }

    result + (template.last -> (result(template.last) + 1))
  }
}
