import scala.io.Source

object Day17 extends App {

  val data = Source
    .fromFile("inputs/input_day17.txt")
    .getLines
    .toList
    .map(_.toInt)
    .sorted
    .reverse

  val k = (1 to data.size).map(combinations(data, _).filter(_.sum == 150))
  println(k.map(_.size).sum)
  println(k.dropWhile(_.isEmpty).head.size)

  def combinations(ls: List[Int], size: Int) = {
    def combinations(ll: Seq[Int], curr: Seq[Int], acc: List[Seq[Int]]): List[Seq[Int]] = {
      if (curr.size == size) acc :+ curr
      else ll.indices.map(i =>
        combinations(
          ll.indices.filter(_ > i).map(ll(_)),
          curr :+ ll(i),
          acc))
        .foldLeft(List[Seq[Int]]())(_ ++ _)
    }

    combinations(ls, Nil, List())
  }

}