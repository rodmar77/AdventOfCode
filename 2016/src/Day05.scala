import java.math.BigInteger
import java.security.MessageDigest

import scala.io.Source

object Day05 {

  def main(args: Array[String]): Unit = {
    val id = Source.fromFile("inputs/2016/input_day05.txt").mkString
    println(getPassword(id))
    println(getPassword2(id))
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

  def getPassword(id: String) = {
    def password(n: Int, p: String): String = {
      val md5 = md5Hash(id + n)
      if (p.length == 8) p
      else if (md5.startsWith("00000")) password(n + 1, p + md5(5))
      else password(n + 1, p)
    }

    password(0, "")
  }

  def getPassword2(id: String) = {
    def password(n: Int, c: Int, p: List[Char]): String = {
      val md5 = md5Hash(id + n)
      val idx = md5(5) - '0'

      if (c == 8) p.mkString
      else if (md5.startsWith("00000")
        && md5(5) >= '0'
        && md5(5) <= '7'
        && p(idx) == ' ')
        password(n + 1, c + 1, p.updated(idx, md5(6)))
      else
        password(n + 1, c, p)
    }

    password(0, 0, List.fill(8)(' '))
  }
}
