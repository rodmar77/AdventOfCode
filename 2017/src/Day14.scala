import Day10.knotHash

import scala.collection.mutable

object Day14 {

  def charToBinaryString(c: Char) = {
    def valueOf = Integer.parseInt(s"$c", 16)

    "%04d".format(valueOf.toBinaryString.toInt)
  }

  def main(args: Array[String]): Unit = {
    val key = "ffayrhll"

    val hashes = (0 until 128)
      .map(i => knotHash(s"$key-$i"))
      .map(_.map(charToBinaryString).mkString)
      .toList

    println(hashes
      .map(
        _.count(_ == '1'))
      .sum)

    println(regionCount(hashes.toArray))
  }

  def regionCount(regions: Array[String]) = {
    def neighbours(y: Int, x: Int): List[(Int, Int)] = {
      ((-1 to 1).map(yinc => (y + yinc, x)) ++ (-1 to 1).map(xinc => (y, x + xinc)))
        .filter {
          case (ny, nx) => regions.isDefinedAt(ny) &&
            regions(ny).isDefinedAt(nx) &&
            regions(ny)(nx) == '1' &&
            (ny != y || nx != x)
        }
        .toList
    }

    def removeRegionRow(row: Int, col: Int): Boolean = {
      val stack = mutable.Stack[(Int, Int)]((row, col))
      while (stack.nonEmpty) {
        stack.pop() match {
          case (y, x) => if (regions(y)(x) == '1') {
            stack.pushAll(neighbours(y, x))
            regions(y) = regions(y).updated(x, '0')
          }
        }
      }

      true
    }

    def removeRegion: Boolean = {
      val firstRow = regions.zipWithIndex.find(_._1.contains('1'))
      if (firstRow.isEmpty) false
      else removeRegionRow(
        firstRow.get._2,
        regions(firstRow.get._2).indexOf('1'))
    }

    def count(acc: Int): Int = {
      if (removeRegion) count(acc + 1)
      else acc
    }

    count(0)
  }
}
