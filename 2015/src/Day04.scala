import java.security._
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

/*

  --- Day 4: The Ideal Stocking Stuffer ---

  Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
  gifts for all the economically forward-thinking little girls and boys.

  To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
  least five zeroes. The input to the MD5 hash is some secret key (your puzzle
  input, given below) followed by a number in decimal. To mine AdventCoins, you
  must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
  that produces such a hash.

  For example:

  If your secret key is abcdef, the answer is 609043, because the MD5 hash of
  abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
  such number to do so.

  If your secret key is pqrstuv, the lowest number it combines with to make an
  MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
  pqrstuv1048970 looks like 000006136ef....

 */
object Day04 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2015/input_day04.txt")) {
      source =>
        val prefix = source.getLines.mkString

        println(minWithPrefixes(prefix, 5))

        // Now find one that starts with six zeroes.
        println(minWithPrefixes(prefix, 6))
    }
  }
  
  def minWithPrefixes(sp: String, count: Int): Int = {
    val md5Digest = MessageDigest.getInstance("MD5")
    def md5Hash(text: String) = md5Digest.digest(text.getBytes("ASCII"))

    def matches(bytes: Array[Byte]): Boolean = {
      @tailrec
      def matches(bytes: Array[Byte], acc: Int): Boolean = {
        if (acc == 0) true
        else if (bytes.head == 0) matches(bytes.tail, acc - 2)
        else if (bytes.head > 0 && bytes.head < 16) acc == 1
        else false
      }

      matches(bytes, count)
    }

    @tailrec
    def minWithPrefix(acc: Int): Int = {
      if (matches(md5Hash(sp + acc))) acc
      else minWithPrefix(acc + 1)
    }

    minWithPrefix(0)
  }

}