import java.math.BigInteger
import java.security.MessageDigest

import scala.collection.mutable

object Day14 {

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def main(args: Array[String]): Unit = {
    val saltPrefix = "ihaygndm"
    println(getPadKey(saltPrefix, 64))
    println(getStretchedPadKey(saltPrefix, 64))
  }

  def getPadKey(saltPrefix: String, keyIndex: Int) = {
    val first = ".*?(.)\\1\\1.*".r

    def isValidPad(start: Int, c: String) =
      (start until start + 1000).exists(x => memoizedMD5Hash(s"$saltPrefix$x").contains(c * 5))

    lazy val memoizedMD5Hash: String => String = memoize { md5Hash }

    def getPadKey(currIndex: Int, keyCount: Int): Int = {
      if (keyCount == keyIndex) currIndex - 1
      else memoizedMD5Hash(s"$saltPrefix$currIndex") match {
        case first(digit) => if (isValidPad(currIndex + 1, digit))
          getPadKey(currIndex + 1, keyCount + 1)
        else
          getPadKey(currIndex + 1, keyCount)
        case _ => getPadKey(currIndex + 1, keyCount)
      }
    }

    getPadKey(0, 0)
  }

  def getStretchedPadKey(saltPrefix: String, keyIndex: Int) = {
    val first = ".*?(.)\\1\\1.*".r

    def stretchHash(acc: String, idx: Int): String = {
      if (idx == 2017) acc
      else stretchHash(md5Hash(acc), idx + 1)
    }

    lazy val memoizedStretchHash: String => String = memoize { stretchHash(_, 0) }

    def isValidPad(start: Int, c: String) =
      (start until start + 1000).exists(x => memoizedStretchHash(s"$saltPrefix$x").contains(c * 5))

    def getPadKey(currIndex: Int, keyCount: Int): Int = {
      if (keyCount == keyIndex) currIndex - 1
      else memoizedStretchHash(s"$saltPrefix$currIndex") match {
        case first(digit) => if (isValidPad(currIndex + 1, digit))
          getPadKey(currIndex + 1, keyCount + 1)
        else
          getPadKey(currIndex + 1, keyCount)
        case _ => getPadKey(currIndex + 1, keyCount)
      }
    }

    getPadKey(0, 0)
  }

  def md5Hash(text: String) = MessageDigest
    .getInstance("MD5")
    .digest(text.getBytes("ASCII"))
    .map("%02x".format(_))
    .mkString
}