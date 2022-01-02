import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day25.txt")) {
      source =>
        val lines = source.getLines().toList
        println(performMoves(lines))
    }
  }

  def performMoves(ocean: List[String]): Int = {
    @tailrec
    def performMoves(currentOcean: List[String], moveCount: Int): Int = {
      val nextOcean = currentOcean
        .map(line => {
          if (line.last == '>' && line.head == '.') '>' +: line.init.tail.replace(">.", ".>") :+ '.'
          else line.replace(">.", ".>")
        })
        .transpose
        .map(_.mkString)
        .map(line => {
          if (line.last == 'v' && line.head == '.') 'v' +: line.init.tail.replace("v.", ".v") :+ '.'
          else line.replace("v.", ".v")
        })
        .transpose
        .map(_.mkString)

      if (currentOcean.indices.forall(i => currentOcean(i).equals(nextOcean(i)))) moveCount
      else performMoves(nextOcean, moveCount + 1)
    }

    performMoves(ocean, 1)
  }
}
