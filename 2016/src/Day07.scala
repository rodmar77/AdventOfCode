import scala.io.Source

/*
  --- Day 7: Internet Protocol Version 7 ---

  While snooping around the local network of EBHQ, you compile a list of IP addresses
  (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out
  which IPs support TLS (transport-layer snooping).

  An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An
  ABBA is any four-character sequence which consists of a pair of two different
  characters followed by the reverse of that pair, such as xyyx or abba. However,
  the IP also must not have an ABBA within any hypernet sequences, which are
  contained by square brackets.

  For example:

    * abba[mnop]qrst supports TLS (abba outside square brackets).

    * abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even
      though xyyx is outside square brackets).

    * aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters
      must be different).

    * ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even
      though it's within a larger string).

 */
object Day07 {

  def main(args: Array[String]): Unit = {
    val ips = Source.fromFile("inputs/2016/input_day07.txt").getLines.toList

    // How many IPs in your puzzle input support TLS?
    println(ips.count(isValidTLS))

    /*
    You would also like to know which IPs support SSL (super-secret listening).

    An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in
    the supernet sequences (outside any square bracketed sections), and a
    corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
    An ABA is any three-character sequence which consists of the same character
    twice with a different character between them, such as xyx or aba. A corresponding
    BAB is the same characters but in reversed positions: yxy and bab, respectively.

    For example:

       * aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab
         within square brackets).

      * xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).

      * aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet;
        the aaa sequence is not related, because the interior character must be
        different).

      * zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a
        corresponding bzb, even though zaz and zbz overlap).

    How many IPs in your puzzle input support SSL?
     */
    println(ips.count(isValidSSL))
  }


  def isValidTLS(ip: String): Boolean = {
    val (containsABBA, containsABBAWithinBrackets) = (
      ".*([a-z0-9])([a-z0-9])\\2\\1.*".r,
      ".*\\[[^]]*([a-z0-9])([a-z0-9])\\2\\1.*".r)

    def validateNotInsideBrackets = ip match {
      case containsABBAWithinBrackets(a, b) => a.equals(b)
      case _ => true
    }

    def validateOutsideBrackets = ip match {
      case containsABBA(a, b) => !a.equals(b)
      case _ => false
    }

    validateNotInsideBrackets && validateOutsideBrackets
  }

  def isValidSSL(ip: String): Boolean = {
    val containsXYX = ".*(.)(.)\\1".r

    def has_XYX_YXY_Matches(insideValues: List[String], outsideValues: List[String]) = {
      def getAllXYX(ll: List[String]) = ll.flatMap(_.sliding(3).map {
        case containsXYX(a, b) => if (a.equals(b)) "" else s"$a$b"
        case _ => ""
      }).filterNot(_.isEmpty)

      (getAllXYX(insideValues), getAllXYX(outsideValues)) match {
        case (inXYX, outXYX) => inXYX.map(_.reverse).exists(outXYX.contains(_))
      }
    }

    has_XYX_YXY_Matches(
      ip.split("\\[[^]]+\\]").toList,
      "\\[([^]]*)\\]".r.findAllIn(ip).matchData.map(_.group(1)).toList)
  }
}
