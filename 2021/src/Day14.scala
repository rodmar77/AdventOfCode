import scala.io.Source
import scala.util.Using

/*
  --- Day 14: Extended Polymerization ---

  The incredible pressures at this depth are starting to put a strain on your submarine.

  The submarine has polymerization equipment that would produce suitable materials to
  reinforce the submarine, and the nearby volcanically-active caves should even have the
  necessary input elements in sufficient quantities.

  The submarine manual contains instructions for finding the optimal polymer formula;
  specifically, it offers a polymer template and a list of pair insertion rules (your
  puzzle input). You just need to work out what polymer would result after repeating the
  pair insertion process a few times.

  For example:

    NNCB

    CH -> B
    HH -> N
    CB -> H
    NH -> C
    HB -> C
    HC -> B
    HN -> C
    NN -> C
    BH -> H
    NC -> B
    NB -> B
    BN -> B
    BB -> N
    BC -> B
    CC -> N
    CN -> C

  The first line is the polymer template - this is the starting point of the process.

  The following section defines the pair insertion rules. A rule like AB -> C means that
  when elements A and B are immediately adjacent, element C should be inserted between
  them. These insertions all happen simultaneously.

  So, starting with the polymer template NNCB, the first step simultaneously considers
  all three pairs:

    The first pair (NN) matches the rule NN -> C, so element C is inserted between the
    first N and the second N.

    The second pair (NC) matches the rule NC -> B, so element B is inserted between the
    N and the C.

    The third pair (CB) matches the rule CB -> H, so element H is inserted between the C
    and the B.

  Note that these pairs overlap: the second element of one pair is the first element of
  the next pair. Also, because all pairs are considered simultaneously, inserted
  elements are not considered to be part of a pair until the next step.

  After the first step of this process, the polymer becomes NCNBCHB.

  Here are the results of a few steps using the above rules:

    Template:     NNCB
    After step 1: NCNBCHB
    After step 2: NBCCNBBBCBHCB
    After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
    After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

  This polymer grows quickly. After step 5, it has length 97; After step 10, it has
  length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times,
  and N occurs 865 times; taking the quantity of the most common element (B, 1749) and
  subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.

  --- Part Two ---

  The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need
  to run more steps of the pair insertion process; a total of 40 steps should do it.

  In the above example, the most common element is B (occurring 2192039569602 times) and
  the least common element is H (occurring 3849876073 times); subtracting these produces
  2188189693529.

 */
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

          //  Apply 10 steps of pair insertion to the polymer template and find the most
          //  and least common elements in the result. What do you get if you take the
          //  quantity of the most common element and subtract the quantity of the least
          //  common element?
          val frequency10 = applyRules(template, rules, 10).values
          println(frequency10.max - frequency10.min)

          // Apply 40 steps of pair insertion to the polymer template and find the most
          // and least common elements in the result. What do you get if you take the
          // quantity of the most common element and subtract the quantity of the least
          // common element?
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
