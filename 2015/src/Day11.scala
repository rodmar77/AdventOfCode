import scala.io.Source

object Day11 extends App {

  val password = Source.fromFile("inputs/2015/input_day11.txt").getLines.mkString
  println(next(password))
  println(next(next(password)))

  def next(s: String) = {
    def matches(s: String) = {
      def matchesFirst = s.sliding(3).map(t => t.map(_ - t.head).mkString == "012").reduce(_ || _)
      def matchesSecond = "lio".forall(s.indexOf(_) < 0)
      def matchesThird = """.*(.)\1.*(.)\2.*""".r.findFirstIn(s).isDefined

      matchesFirst && matchesSecond && matchesThird
    }

    def inc(s: String) = {
      def inc(curr: String, acc: String): String = {
        if (curr.last == 'z') inc(curr.init, 'a' + acc)
        else curr.init + (curr.last + 1).toChar + acc
      }

      inc(s, "")
    }

    def _next(curr: String): String = {
      if (matches(curr)) curr
      else _next(inc(curr))
    }

    _next(inc(s))
  }

}
