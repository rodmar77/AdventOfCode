object Day11 extends App {

  println(next("hxbxwxba"))
  println(next(next("hxbxwxba")))

  def next(s: String) = {
    def matches(s: String) = {
      def matchesFirst = s.sliding(3).map(t => (t(2) - t(1) == 1) && (t(1) - t(0) == 1)).count(_ == true) >= 1
      def matchesSecond = s.count(c => (c == 'l') || (c == 'i') || (c == 'o')) == 0
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
