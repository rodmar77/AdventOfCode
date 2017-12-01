import scala.io.Source

object Day24 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day24.txt")
    .getLines
    .map(BigInt(_))
    .toList

  def result(groupCount: Int) = {
    def partition(partitionSize: Int): List[BigInt] = {
      if (partitionSize == data.size) Nil
      else {
        val all = data
                    .combinations(partitionSize)
                    .filter(_.sum == data.sum/groupCount)
                    .toList

        if (all.isEmpty) partition(partitionSize + 1)
        else all.foldLeft(all.head)((a, b) => if (a.product < b.product) a else b)
      }
    }

    partition(1)
  }

  println(result(3).product)
  println(result(4).product)

}
