import scala.io.Source

object Day07 {

  def main(args: Array[String]): Unit = {
    val ips = Source.fromFile("inputs/2016/input_day07.txt").getLines.toList

    println(ips.count(isValidTLS))
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
        case containsXYX(a, b) => if (a.equals(b)) "" else s"$a$b$a"
        case _ => ""
      }).filterNot(_.isEmpty)

      val (insideXYX, outsideXYX) = (getAllXYX(insideValues), getAllXYX(outsideValues))
      insideXYX.exists(ix => outsideXYX.exists(ox => ix(0) == ox(1) && ix(1) == ox(0)))
    }

    has_XYX_YXY_Matches(
      ip.split("\\[[^]]+\\]").toList,
      "\\[([^]]*)\\]".r.findAllIn(ip).matchData.map(_.group(1)).toList)
  }
}
