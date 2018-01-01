import scala.io.Source

/*

  --- Day 15: Science for Hungry People ---

  Today, you set out on the task of perfecting your milk-dunking cookie recipe.
  All you have to do is find the right balance of ingredients.

  Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
  list of the remaining ingredients you could use to finish the recipe (your
  puzzle input) and their properties per teaspoon:

  * capacity (how well it helps the cookie absorb milk)
  * durability (how well it keeps the cookie intact when full of milk)
  * flavor (how tasty it makes the cookie)
  * texture (how it improves the feel of the cookie)
  * calories (how many calories it adds to the cookie)

  You can only measure ingredients in whole-teaspoon amounts accurately, and
  you have to be accurate so you can reproduce your results in the future.
  The total score of a cookie can be found by adding up each of the properties
  (negative totals become 0) and then multiplying together everything except
  calories.

  For instance, suppose you have these two ingredients:

  Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
  Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
  Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of
  cinnamon (because the amounts of each ingredient must add up to 100) would
  result in a cookie with the following properties:

  * A capacity of 44*-1 + 56*2 = 68
  * A durability of 44*-2 + 56*3 = 80
  * A flavor of 44*6 + 56*-2 = 152
  * A texture of 44*3 + 56*-1 = 76

  Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
  results in a total score of 62842880, which happens to be the best score possible
  given these ingredients. If any properties had produced a negative total, it
  would have instead become zero, causing the whole score to multiply to zero.
 */
object Day15 extends App {

  case class Recipe(capacity: BigInt, durability: BigInt, flavor: BigInt, texture: BigInt, calories: BigInt) {
    def *(mult: BigInt) = Recipe(
      mult * this.capacity,
      mult * this.durability,
      mult * this.flavor,
      mult * this.texture,
      mult * this.calories)

    def +(that: Recipe) = Recipe(
      this.capacity + that.capacity,
      this.durability + that.durability,
      this.flavor + that.flavor,
      this.texture + that.texture,
      this.calories + that.calories)

    def score: Score = Score(score(capacity, durability, flavor, texture), calories)

    private def score(a: BigInt, b: BigInt*): BigInt = {
      if (a <= 0) 0
      else if (b.isEmpty) a
      else if (b.head <= 0) 0
      else score(a * b.head, b.tail: _*)
    }
  }

  case class Score(score: BigInt, calories: BigInt) {
    override def toString: String = score.toString
  }

  val regex = """.+?: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  val data = Source
    .fromFile("inputs/2015/input_day15.txt")
    .getLines
    .map {
      case regex(capacity, durability, flavor, texture, calories) =>
        Recipe(BigInt(capacity), BigInt(durability), BigInt(flavor), BigInt(texture), BigInt(calories))
    }
    .toList
    .permutations
    .toList

  // Given the ingredients in your kitchen and their properties, what is the total
  // score of the highest-scoring cookie you can make?
  println(partitionInto(100, 4).flatMap(points).maxBy(_.score))

  /*

  Your cookie recipe becomes wildly popular! Someone asks if you can make another
  recipe that has exactly 500 calories per cookie (so they can use it as a meal
  replacement). Keep the rest of your award-winning process the same (100 teaspoons,
  same ingredients, same scoring system).

  For example, given the ingredients above, if you had instead selected 40 teaspoons
  of butterscotch and 60 teaspoons of cinnamon (which still adds to 100), the total
  calorie count would be 40*8 + 60*3 = 500. The total score would go down, though:
  only 57600000, the best you can do in such trying circumstances.

  Given the ingredients in your kitchen and their properties, what is the total
  score of the highest-scoring cookie you can make with a calorie total of 500?

  */
  println(partitionInto(100, 4).flatMap(points).filter(_.calories == BigInt(500)).maxBy(_.score))

  def points(part: List[BigInt]): List[Score] = {
    def points(cond: List[Recipe]) = {
      (part zip cond).map {
        case (mult, r) => r * mult
      }
        .reduce(_ + _)
        .score
    }

    data
      .map(points)
  }

  def partitionInto(number: Int, groups: Int) = {
    def partitionInto(sum: Int, curr: List[BigInt], acc: Set[List[BigInt]]): Set[List[BigInt]] = {
      if (curr.size == groups - 1) acc + (curr :+ BigInt(number - sum)).sorted
      else (1 to number - sum - (groups - curr.size)).map(n => partitionInto(sum + n, curr :+ BigInt(n), acc)).reduce(_ ++ _)
    }

    partitionInto(0, Nil, Set())
  }
}
