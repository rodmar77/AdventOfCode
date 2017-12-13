import java.math.BigInteger
import java.security._

import scala.io.Source

object Day04 extends App {

  val prefix = Source.fromFile("inputs/2015/input_day04.txt").getLines.mkString

  println(minWithPrefixes(prefix, "00000"))
  println(minWithPrefixes(prefix, "000000"))
  
  def minWithPrefixes(sp: String, hp: String) = {
    def minWithPrefix(acc: Int): Int = {
      if (md5Hash(sp + acc).startsWith(hp)) acc
      else minWithPrefix(acc + 1)
    }

    def md5Hash(text: String) = {
      val m = MessageDigest.getInstance("MD5")
      val b = text.getBytes("UTF-8")
      m.update(b, 0, b.length)
      new BigInteger(1, m.digest())
        .toString(16)
        .reverse
        .padTo(32, "0")
        .reverse
        .mkString
    }

    minWithPrefix(0)
  }

}