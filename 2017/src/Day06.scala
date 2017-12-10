import scala.io.Source

object Day06 {

  def main(args: Array[String]): Unit = {
    val banks = Source
      .fromFile("inputs/2017/input_day06.txt")
      .mkString
      .split("\\s+")
      .map(_.toInt)
      .toList

    println(countSteps(banks))
    println(countStepDiff(banks))
  }

  private def countSteps(array: List[Int]) = {
    def update(array: List[Int]): List[Int] = {
      val (idx, total) = (array.indexOf(array.max), array.max)
      updateByIndex(array.updated(idx, 0), (idx + 1) % array.length, total)
    }

    def updateByIndex(array: List[Int], idx: Int, total: Int): List[Int] = {
      if (total == 0) array
      else updateByIndex(array.updated(idx, array(idx) + 1), (idx + 1) % array.length, total - 1)
    }

    def countSteps(array: List[Int], acc: Int, s: Set[String]): Int = {
      if (s.contains(array.mkString("-"))) acc
      else countSteps(update(array), acc + 1, s + array.mkString("-"))
    }

    countSteps(array, 0, Set())
  }

  private def countStepDiff(array: List[Int]) = {
    def update(array: List[Int]): List[Int] = {
      val (idx, total) = (array.indexOf(array.max), array.max)
      updateByIndex(array.updated(idx, 0), (idx + 1) % array.length, total)
    }

    def updateByIndex(array: List[Int], idx: Int, total: Int): List[Int] = {
      if (total == 0) array
      else updateByIndex(array.updated(idx, array(idx) + 1), (idx + 1) % array.length, total - 1)
    }

    def countSteps(array: List[Int], acc: Int, s: List[String]): Int = {
      val key = array.mkString("-")
      if (s.contains(key)) acc - s.indexOf(key)
      else countSteps(update(array), acc + 1, s :+ key)
    }

    countSteps(array, 0, List())
  }
}
