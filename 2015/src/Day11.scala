import scala.io.Source

/*

  --- Day 11: Corporate Policy ---

  Santa's previous password expired, and he needs help choosing a new one.

  To help him remember his new password after the old one expires, Santa has devised
  a method of coming up with a password based on the previous one. Corporate policy
  dictates that passwords must be exactly eight lowercase letters (for security reasons),
  so he finds his new password by incrementing his old password string repeatedly until
  it is valid.

  Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
  Increase the rightmost letter one step; if it was z, it wraps around to a, and
  repeat with the next letter to the left until one doesn't wrap around.

  Unfortunately for Santa, a new Security-Elf recently started, and he has imposed
  some additional password requirements:

  * Passwords must include one increasing straight of at least three letters, like abc,
    bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.

  * Passwords may not contain the letters i, o, or l, as these letters can be mistaken
    for other characters and are therefore confusing.

  * Passwords must contain at least two different, non-overlapping pairs of letters,
    like aa, bb, or zz.

  For example:

  * hijklmmn meets the first requirement (because it contains the straight hij) but
    fails the second requirement requirement (because it contains i and l).

  * abbceffg meets the third requirement (because it repeats bb and ff) but fails the
    first requirement.

  * abbcegjk fails the third requirement, because it only has one double letter (bb).

  The next password after abcdefgh is abcdffaa.
  The next password after ghijklmn is ghjaabcc, because you eventually skip all
  the passwords that start with ghi..., since i is not allowed.

 */
object Day11 extends App {

  val password = Source.fromFile("inputs/2015/input_day11.txt").getLines.mkString

  // Given Santa's current password (your puzzle input), what should his next password be?
  val nextPassword = next(password)
  println(nextPassword)

  // Santa's password expired again. What's the next one?
  println(next(nextPassword))

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
