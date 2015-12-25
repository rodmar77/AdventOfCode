import Math.max
import scala.io.Source

object Day21 extends App {

  val regex = """(.+?): (\d+)""".r

  val data = Source
    .fromFile("inputs/input_day21.txt")
    .getLines
    .map {
      case regex(name, value) => name -> value.toInt
    }
    .toMap

  println(combinations.map(result(_)(true, false)).filter(_._2).minBy(_._1)._1)
  println(combinations.map(result(_)(false, true)).filter(_._2).maxBy(_._1)._1)

  def result(ls: List[(Int, Int, Int)])(f: (Boolean, Boolean)): (Int, Boolean) = {
    val (cost, damage, armor) = (ls.map(_._1).sum, ls.map(_._2).sum, ls.map(_._3).sum)

    def result(player: Int, boss: (Int, Int, Int)): Boolean = {
      if (boss._1 <= 0) f._1
      else if (player <= 0) f._2
      else result(player - max(boss._2 - armor, 1), (boss._1 - max(damage - boss._3, 1), boss._2, boss._3))
    }

    (cost, result(100, (data("Hit Points"), data("Damage"), data("Armor"))))
  }

  def combinations = {
    val weapons = List((8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0))
    val armors = List((13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5))
    val rings = List((25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3))

    (for (
      weapon <- weapons;
      armor <- armors;
      ringOne <- rings;
      ringTwo <- rings
      if ringOne != ringTwo
    ) yield List(List(weapon, armor, ringOne, ringTwo), List(weapon, armor, ringOne), List(weapon, armor)))
      .flatten
      .distinct
  }

}
