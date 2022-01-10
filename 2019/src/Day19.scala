import util.IntCodeMachine

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object Day19 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2019/input_day19.txt")) {
      source =>
        val numbers = source
          .mkString
          .split(",")
          .map(BigInt(_))
          .toList

        println(getCoords(50, 50, numbers).count {
          case (_, _, v) => v
        })

        println(findSquare(100, 100, numbers) match {
          case (x, y) => 10000*x + y
        })
    }
  }

  private def findSquare(width: Int, height: Int, numbers: List[BigInt]) = {
    def buba(startY: Int, xPos: (Int, Int)) = (startY + 1 to 2000).scanLeft((startY - 1, xPos)) {
      case ((_, (start, end)), y) =>
        (y, (
          (start to Int.MaxValue).find(isOn(numbers, _, y)) match {
            case Some(x) => x
          },
          (end to Int.MaxValue).find(!isOn(numbers, _, y)) match {
            case Some(x) => x - 1
          })
        )
    }

    val limits = getCoords(30, 30, numbers)
      .groupBy {
        case (y, _, _) => y
      }.toList
       .sortBy {
        case (y, _) => y
      }.map {
        case (_, list) => list.map {
          case (_, _, v) => if (v) '#' else '.'
        }.mkString
      }.zipWithIndex.find {
        case (line, _) => line.contains("###")
      } match {
        case Some((line, y)) => buba(y, (line.indexOf('#'), line.lastIndexOf('#')))
      }

    limits.find {
      case (y1, (_, e1)) => limits.exists {
        case (y2, (s2, _)) => y2 - y1 == height - 1 && e1 - s2 == width - 1
      }
    } match {
      case Some((y, (_, e))) => (e - width + 1, y)
    }
  }

  private def isOn(numbers: List[BigInt], x: Int, y: Int) = {
    val lb = ListBuffer[BigInt]()
    val machine = IntCodeMachine(lb.append)
    machine.start(numbers)
    machine.resume(x, y)
    lb.last == 1
  }

  private def getCoords(width: Int, height: Int, numbers: List[BigInt]) = {
    (0 until height).flatMap(y => (0 until width).map(x => (y, x, isOn(numbers, x, y))))
  }
}
