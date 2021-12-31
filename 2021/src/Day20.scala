import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day20 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day20.txt")) {
      source =>
        val ll = source.getLines().toList
        val (reference, matrix) = (ll.head, ll.tail.tail.map(_.toList))

        println(getEnhancedImage(reference, 2, matrix).map(_.count(_ == '#')).sum)
        println(getEnhancedImage(reference, 50, matrix).map(_.count(_ == '#')).sum)
    }
  }

  @tailrec
  def getEnhancedImage(reference: String, count: Int, matrix: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    def referenceValueOf(value: Seq[Seq[Char]]) = {
      def m = Map[Char, Char]('.'->'0', '#'->'1')
      reference(Integer.parseInt(value.flatMap(_.map(m)).mkString, 2))
    }

    def getSubMatrixCenteredAt(x: Int, y: Int, matrix: Seq[Seq[Char]]) = {
      def getDefaultValue = if (count % 2 == 0) '.' else '#'
      def getValueAt(xi: Int, yi: Int) = {
        if (yi < 0 || xi < 0 || yi >= matrix.length || xi >= matrix(yi).length) getDefaultValue
        else matrix(yi)(xi)
      }

      (-1 to 1).map(yInc => (-1 to 1).map(xInc => getValueAt(x + xInc, y + yInc)))
    }

    if (count == 0) matrix
    else getEnhancedImage(reference, count - 1, (-1 to matrix.length).map(y => (-1 to matrix.head.length).map(x => {
      referenceValueOf(getSubMatrixCenteredAt(x, y, matrix))
    })))
  }
}
