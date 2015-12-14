import java.security._

object Day4 extends App {

  println(minWithPrefix("00000"))
  println(minWithPrefix("000000"))
  
  def minWithPrefix(p: String) = Range(1, Int.MaxValue).indexWhere(i => hash("ckczppom" + i).startsWith(p))

  def hash(s: String) = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(s.getBytes).map(b => {
      if ((0xff & b) < 0x10) "0" + (0xFF & b).toHexString
      else (0xFF & b).toHexString
    }).mkString
  }

}