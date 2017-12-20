import java.security.MessageDigest

import scala.collection.mutable

object Day14 {

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def main(args: Array[String]): Unit = {
    val saltPrefix = "ihaygndm"
    println(PadCrypto(saltPrefix).getKey(64))
    println(StretchedPadCrypto(saltPrefix).getKey(64))
  }

  abstract class Crypto(val saltPrefix: String) {
    private val first = ".*?(.)\\1\\1.*".r

    def isValidPad(start: Int, c: String) =
      (start until start + 1000).exists(index => memoizedHash(s"$saltPrefix$index").contains(c * 5))

    def getKey(keyIndex: Int) = {
      def getKey(currIndex: Int, keyCount: Int): Int = {
        if (keyCount == keyIndex) currIndex - 1
        else memoizedHash(s"$saltPrefix$currIndex") match {
          case first(digit) => if (isValidPad(currIndex + 1, digit))
            getKey(currIndex + 1, keyCount + 1)
          else
            getKey(currIndex + 1, keyCount)
          case _ => getKey(currIndex + 1, keyCount)
        }
      }

      getKey(0, 0)
    }

    def md5Hash(text: String) = MessageDigest
      .getInstance("MD5")
      .digest(text.getBytes("ASCII"))
      .map("%02x".format(_))
      .mkString

    def memoizedHash: String => String
  }

  case class PadCrypto(sp: String) extends Crypto(sp) {
    lazy val memoizedHash: String => String = memoize { md5Hash }
  }

  case class StretchedPadCrypto(sp: String) extends Crypto(sp) {
    private def stretchHash(text: String) = {
      def stretchHash(acc: String, idx: Int): String = {
        if (idx == 2017) acc
        else stretchHash(md5Hash(acc), idx + 1)
      }

      stretchHash(text, 0)
    }

    lazy val memoizedHash: String => String = memoize { stretchHash }
  }
}