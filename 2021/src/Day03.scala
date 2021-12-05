import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

/*
  --- Day 3: Binary Diagnostic ---

  The submarine has been making some odd creaking noises, so you ask it to produce a
  diagnostic report just in case.

  The diagnostic report (your puzzle input) consists of a list of binary numbers which,
  when decoded properly, can tell you many useful things about the conditions of the
  submarine. The first parameter to check is the power consumption.

  You need to use the binary numbers in the diagnostic report to generate two new binary
  numbers (called the gamma rate and the epsilon rate). The power consumption can then
  be found by multiplying the gamma rate by the epsilon rate.

  Each bit in the gamma rate can be determined by finding the most common bit in the
  corresponding position of all numbers in the diagnostic report. For example, given the
  following diagnostic report:

    00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010

  Considering only the first bit of each number, there are five 0 bits and seven 1 bits.
  Since the most common bit is 1, the first bit of the gamma rate is 1.

  The most common second bit of the numbers in the diagnostic report is 0, so the second
  bit of the gamma rate is 0.

  The most common value of the third, fourth, and fifth bits are 1, 1, and 0,
  respectively, and so the final three bits of the gamma rate are 110.

  So, the gamma rate is the binary number 10110, or 22 in decimal.

  The epsilon rate is calculated in a similar way; rather than use the most common bit,
  the least common bit from each position is used. So, the epsilon rate is 01001, or 9
  in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power
  consumption, 198.

  Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon
  rate, then multiply them together. What is the power consumption of the submarine? (Be
  sure to represent your answer in decimal, not binary.)

 */
object Day03 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day03.txt")) {
      source =>
        val numbers = source.getLines.toList
        val digits = (0 until 12).map(digit => numbers.map(_.charAt(digit)).groupBy(n => n))
        val mostFrequent = digits.map(_.maxBy {
            case (_, v) => v.length
          }).map {
            case (k, _) => k
          }.mkString

        val lessFrequent = digits.map(_.minBy {
            case (_, v) => v.length
          }).map {
            case (k, _) => k
          }.mkString

        println(Integer.parseInt(mostFrequent, 2) * Integer.parseInt(lessFrequent, 2))
        frequencyWithFiltering(numbers) match {
          case (mostFrequent, lessFrequent) => println(mostFrequent * lessFrequent)
        }
    }
  }

  def frequencyWithFiltering(ns: List[String]): (Int, Int) = {

    def getDigits(xs: List[String], idx:Int) = xs
      .map(_.charAt(idx))
      .groupBy(n => n)
      .view
      .mapValues(_.length)
      .toMap
      .withDefaultValue(0)

    @tailrec
    def mostFrequentWithFiltering(xs: List[String], idx: Int): String = {
      if (xs.length == 1) xs.head
      else {
        val digits = getDigits(xs, idx)
        if (digits('1') >= digits('0')) mostFrequentWithFiltering(xs.filter(_.charAt(idx) == '1'), idx + 1)
        else mostFrequentWithFiltering(xs.filter(_.charAt(idx) == '0'), idx + 1)
      }
    }

    @tailrec
    def lessFrequentWithFiltering(xs: List[String], idx: Int): String = {
      if (xs.length == 1) xs.head
      else {
        val digits = getDigits(xs, idx)
        if (digits('0') <= digits('1')) lessFrequentWithFiltering(xs.filter(_.charAt(idx) == '0'), idx + 1)
        else lessFrequentWithFiltering(xs.filter(_.charAt(idx) == '1'), idx + 1)
      }
    }

    (Integer.parseInt(mostFrequentWithFiltering(ns, 0), 2),
     Integer.parseInt(lessFrequentWithFiltering(ns, 0), 2))
  }

}
