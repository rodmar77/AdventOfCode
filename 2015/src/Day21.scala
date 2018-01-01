import java.lang.Math.max

import scala.io.Source

/*

  --- Day 21: RPG Simulator 20XX ---

  Little Henry Case got a new video game for Christmas. It's an RPG, and he's
  stuck on a boss. He needs to know what equipment to buy at the shop. He hands
  you the controller.

  In this game, the player (you) and the enemy (the boss) take turns attacking.
  The player always goes first. Each attack reduces the opponent's hit points
  by at least 1. The first character at or below 0 hit points loses.

  Damage dealt by an attacker each turn is equal to the attacker's damage score
  minus the defender's armor score. An attacker always does at least 1 damage.
  So, if the attacker has a damage score of 8, and the defender has an armor
  score of 3, the defender loses 5 hit points. If the defender had an armor
  score of 300, the defender would still lose 1 hit point.

  Your damage score and armor score both start at zero. They can be increased by
  buying items in exchange for gold. You start with no items and have as much gold
  as you need. Your total damage or armor is equal to the sum of those stats from
  all of your items. You have 100 hit points.

  Here is what the item shop is selling:

  +---------------------------------+
  | Weapons:    Cost  Damage  Armor |
  | Dagger        8     4       0   |
  | Shortsword   10     5       0   |
  | Warhammer    25     6       0   |
  | Longsword    40     7       0   |
  | Greataxe     74     8       0   |
  |                                 |
  | Armor:      Cost  Damage  Armor |
  | Leather      13     0       1   |
  | Chainmail    31     0       2   |
  | Splintmail   53     0       3   |
  | Bandedmail   75     0       4   |
  | Platemail   102     0       5   |
  |                                 |
  | Rings:      Cost  Damage  Armor |
  | Damage +1    25     1       0   |
  | Damage +2    50     2       0   |
  | Damage +3   100     3       0   |
  | Defense +1   20     0       1   |
  | Defense +2   40     0       2   |
  | Defense +3   80     0       3   |
  +---------------------------------+

  You must buy exactly one weapon; no dual-wielding. Armor is optional, but you
  can't use more than one. You can buy 0-2 rings (at most one for each hand). You
  must use any items you buy. The shop only has one of each item, so you can't
  buy, for example, two rings of Damage +3.

  For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that
  the boss has 12 hit points, 7 damage, and 2 armor:

  * The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
  * The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
  * The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
  * The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
  * The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
  * The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
  * The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

  In this scenario, the player wins! (Barely.)

 */
object Day21 extends App {

  val regex = """(.+?): (\d+)""".r

  val data = Source
    .fromFile("inputs/2015/input_day21.txt")
    .getLines
    .map {
      case regex(name, value) => name -> value.toInt
    }
    .toMap

  // You have 100 hit points. The boss's actual stats are in your puzzle input.
  // What is the least amount of gold you can spend and still win the fight?
  println(combinations.map(result(_)(true, false)).filter(_._2).minBy(_._1)._1)

  // Turns out the shopkeeper is working with the boss, and can persuade you
  // to buy whatever items he wants. The other rules still apply, and he still
  // only has one of each item. What is the most amount of gold you can spend
  // and still lose the fight?
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
