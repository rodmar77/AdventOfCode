import scala.io.Source

object Day21 {

  def main(args: Array[String]): Unit = {
    val transformation = "(.+) => (.+)".r
    val transformations = Source
      .fromFile("inputs/2017/input_day21.txt")
      .getLines
      .map {
        case transformation(src, target) => Transformation(
          src
            .split("/")
            .toList,
          target
            .split("/")
            .toList)

      }.toList

    println(iterate(".#./..#/###", transformations, 5).count(_ == '#'))
    println(iterate(".#./..#/###", transformations, 18).count(_ == '#'))
  }

  case class Transformation(src: List[String], target: List[String]) {
    def flip(t: List[String]) = t.reverse
    def rotate(t: List[String], count: Int) = {
      def rot(acc: List[String], idx: Int): List[String] = {
        if (idx == count) acc
        else rot(acc.transpose.map(_.reverse.mkString), idx + 1)
      }

      rot(t, 0)
    }

    private val sources = List
      .tabulate(4)(n => List(
        rotate(src, n),
        flip(rotate(src, n))))
      .flatten
      .map(_.mkString("/"))

    def canTransform(v: String) = sources.contains(v)
  }

  def split(board: String) = {
    def splitArea(ll: List[String], size: Int) = ll
      .map(_.sliding(size, size).toList)
      .transpose
      .map(_.mkString("/"))

    def split(board: List[String], size: Int) = board
        .sliding(size, size)
        .flatMap(splitArea(_, size))
        .toList

    val splitBoard = board.split("/").toList
    if (splitBoard.size % 2 == 0)
      split(splitBoard, 2)
    else
      split(splitBoard, 3)
  }

  def join(k: List[List[String]]) = {
    val size = Math.sqrt(k.size).toInt
    k
      .sliding(size, size)
      .map(_.transpose.map(_.mkString))
      .map(_.mkString("/"))
      .mkString("/")
  }

  def iterate(start: String, transformations: List[Transformation], rounds: Int) = {
    def iterate(acc: String, current: Int): String = {
      if (current == rounds) acc
      else {
        val splits = split(acc).map(pt => transformations.find(_.canTransform(pt)) match {
          case Some(transf) => transf.target
       })

        iterate(
          join(splits),
          current + 1)
      }
    }

    transformations.find(_.canTransform(start)) match {
      case Some(transf) => iterate(transf.target.mkString("/"), 1)
    }
  }
}
