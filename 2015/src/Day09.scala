import scala.io._

/*
  --- Day 9: All in a Single Night ---

  Every year, Santa manages to deliver all of his presents in a single night.

  This year, however, he has some new locations to visit; his elves have provided
  him the distances between every pair of locations. He can start and end at any
  two (different) locations he wants, but he must visit each location exactly once.
  What is the shortest distance he can travel to achieve this?

  For example, given the following distances:

  London to Dublin = 464
  London to Belfast = 518
  Dublin to Belfast = 141

  The possible routes are therefore:

  Dublin -> London -> Belfast = 982
  London -> Dublin -> Belfast = 605
  London -> Belfast -> Dublin = 659
  Dublin -> Belfast -> London = 659
  Belfast -> Dublin -> London = 605
  Belfast -> London -> Dublin = 982

  The shortest of these is London -> Dublin -> Belfast = 605, and so the answer
  is 605 in this example.

 */
object Day09 extends App {

  val links = Source
    .fromFile("inputs/2015/input_day09.txt")
    .getLines
    .map(_.split(" "))
    .map(arr => (arr(0), arr(2)) -> arr(4).toInt)
    .flatMap(p => Seq(p, (p._1.swap, p._2)))
    .toMap

  // What is the distance of the shortest route?
  println(route(links).min)

  /*

  The next year, just to show off, Santa decides to take the route with the longest
  distance instead.

  He can still start and end at any two (different) locations he wants, and he
  still must visit each location exactly once.

  For example, given the distances above, the longest route would be 982 via
  (for example) Dublin -> London -> Belfast.

  What is the distance of the longest route?

   */
  println(route(links).max)

  def route(vs: Map[(String, String), Int]) = {
    val names = vs.keys.map(_._1).toList.distinct
    names.permutations.map(_.sliding(2).map(l => vs((l.head, l.last))).sum)
  }
}