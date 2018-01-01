import scala.io.Source

/*

  --- Day 17: No Such Thing as Too Much ---

  The elves bought too much eggnog again - 150 liters this time. To fit it all
  into your refrigerator, you'll need to move it into smaller containers. You
  take an inventory of the capacities of the available containers.

  For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
  If you need to store 25 liters, there are four ways to do it:

  * 15 and 10
  * 20 and 5 (the first 5)
  * 20 and 5 (the second 5)
  * 15, 5, and 5

 */
object Day17 extends App {

  val data = Source
    .fromFile("inputs/2015/input_day17.txt")
    .getLines
    .toList
    .map(_.toInt)
    .sorted
    .reverse

  // Filling all containers entirely, how many different combinations of containers
  // can exactly fit all 150 liters of eggnog?
  val k = (1 to data.size).map(combinations(data, _).filter(_.sum == 150))
  println(k.map(_.size).sum)

  /*

  While playing with all the containers in the kitchen, another load of eggnog
  arrives! The shipping and receiving department is requesting as many containers
  as you can spare.

  Find the minimum number of containers that can exactly fit all 150 liters of
  eggnog. How many different ways can you fill that number of containers and
  still hold exactly 150 litres?

  In the example above, the minimum number of containers was two. There were three
  ways to use that many containers, and so the answer there would be 3.

   */
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