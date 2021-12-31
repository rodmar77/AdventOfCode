import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.{Map => MMap}

object Day15 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day15.txt")) {
      source =>
        val matrix = source.getLines().map(_.toList.map(_ - '0')).toList
        println(getMinimumSumPath(matrix))
        println(getMinimumSumPath(augmentMatrix(matrix)))
    }
  }

  private def augmentMatrix(matrix: List[List[Int]]) = {
    val augmented = (0 to 10).scanLeft(matrix) {
      case (m, _) => m.map(_.map(x => if (x == 9) 1 else x + 1))
    }

    (0 until 5).toList.flatMap(n => augmented.slice(n, n + 5).reduceRight[List[List[Int]]] {
      case (a, b) => a.indices.map(x => a(x) ++ b(x)).toList
    })
  }

  def getMinimumSumPath(matrix: List[List[Int]]): Int = {
    def neighbors(pos: (Int, Int)) = pos match {
      case (y, x) => List((1, 0), (-1, 0), (0, 1), (0, -1)).map {
        case (yinc, xinc) => (y + yinc, x + xinc)
      }.filter {
        case (y, x) => y >= 0 && x >= 0 && y < matrix.length && x < matrix(y).length
      }
    }

    def dijkstra() = {
      val dists = MMap[(Int, Int), Int]((0, 0) -> 0).withDefaultValue(Int.MaxValue / 2)

      @tailrec
      def process(ll: Set[(Int, Int)]): MMap[(Int, Int), Int] =
        if (ll.isEmpty) dists
        else {
          val smallest = dists
            .filterKeys(ll)
            .minBy {
            case (_, value) => value
          } match {
            case (key, _) => key
          }

          println(ll.size)
          neighbors(smallest).foreach {
            case (y, x) =>
              val newDist = dists(smallest) + matrix(y)(x)
              if (newDist < dists((y, x))) dists.put((y, x), newDist)
          }

          process(ll diff Set(smallest))
      }

      process(matrix.indices.flatMap(y => matrix(y).indices.map(x => (y, x))).toSet)
    }

    dijkstra()((matrix.length - 1, matrix.head.length - 1))
  }
}
