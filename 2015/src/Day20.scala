import java.lang.Math.sqrt

import scala.io.Source

/*
  --- Day 20: Infinite Elves and Infinite Houses ---

  To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door.
  He sends them down a street with infinite houses numbered sequentially: 1, 2, 3, 4,
  5, and so on.

  Each Elf is assigned a number, too, and delivers presents to houses based on
  that number:

  * The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
  * The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
  * Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....

  There are infinitely many Elves, numbered starting with 1. Each Elf delivers
  presents equal to ten times his or her number at each house.

  So, the first nine houses on the street end up like this:

  +---------------------------+
  | House 1 got 10 presents.  |
  | House 2 got 30 presents.  |
  | House 3 got 40 presents.  |
  | House 4 got 70 presents.  |
  | House 5 got 60 presents.  |
  | House 6 got 120 presents. |
  | House 7 got 80 presents.  |
  | House 8 got 150 presents. |
  | House 9 got 130 presents. |
  +---------------------------+

  The first house gets 10 presents: it is visited only by Elf 1, which delivers
  1 * 10 = 10 presents. The fourth house gets 70 presents, because it is visited
  by Elves 1, 2, and 4, for a total of 10 + 20 + 40 = 70 presents.

   */
object Day20 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day20.txt")
    .getLines
    .mkString
    .toInt

  // What is the lowest house number of the house to get at least as many presents
  // as the number in your puzzle input?
  println(Range(1, Int.MaxValue).dropWhile(sm(_) * 10 <= data).head)

  // The Elves decide they don't want to visit an infinite number of houses. Instead,
  // each Elf will stop after delivering presents to 50 houses. To make up for it,
  // they decide to deliver presents equal to eleven times their number at each house.
  //
  // With these changes, what is the new lowest house number of the house to get at
  // least as many presents as the number in your puzzle input?
  println(Range(1, Int.MaxValue).dropWhile(n => sm(n)(_ * 50d >= n) * 11 <= data).head)

  def sm(n: Int)(implicit f: Int => Boolean = _ => true) = {
    def isPrime(x: Int) = (x == 2) || ((x % 2 != 0) && !(3 to sqrt(x).toInt by 2).exists(x % _ == 0))

    def nextPrime(x: Int) = {
      def np(nc: Int): Int = if (isPrime(nc)) nc else np(nc + 2)

      if ((x % 2) == 0) np(x + 1) else np(x + 2)
    }

    def factor(x: Int) = {
      def divPower(x: Int, p: Int) = {
        def divPower(x: Int, acc: Int, accp: Int): (Int, Int) = {
          if (x % p == 0) divPower(x / p, acc + 1, accp * p)
          else (acc, accp)
        }

        divPower(x, 0, 1)
      }

      def factor(x: Int, p: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
        if (x == 1) acc
        else if (isPrime(x)) acc :+ (x, 1)
        else if (x % p == 0) divPower(x, p) match {
          case (pow, v) => factor(x / v, nextPrime(p), acc :+ (p, pow))
        } else factor(x, nextPrime(p), acc)
      }

      factor(x, 2, Nil)
    }

    def multiples = {
      def multiples(ll: List[(Int, Int)], curr: Int): Seq[Int] = {
        if (ll.isEmpty) List(curr)
        else ll.head match {
          case (prime, pow) => (0 to pow).flatMap(p => multiples(ll.tail, curr * Math.pow(prime, p).toInt))
        }
      }

      multiples(factor(n), 1)
    }

    multiples.filter(f).sum
  }
}
