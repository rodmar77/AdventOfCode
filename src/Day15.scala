import scala.io.Source

object Day15 extends App {

  val regex = """.+?: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  val data = Source
    .fromFile("inputs/input_day15.txt")
    .getLines
    .map {
      case regex(capacity, durability, flavor, texture, calories) =>
        (BigInt(capacity), BigInt(durability), BigInt(flavor), BigInt(texture), BigInt(calories))
    }
    .toList
    .permutations
    .toList

  println(partitionInto(100, 4).map(points(_, n => true)).max)
  println(partitionInto(100, 4).map(points(_, _ == BigInt(500))).max)

  def points(part: List[BigInt], calFun: BigInt => Boolean) = {
    def max(a: BigInt, b: BigInt) = if (a > b) a else b
    def m(a: BigInt, b: BigInt*): BigInt = {
      if (a <= 0) 0
      else if (b.isEmpty) a
      else if (b.head <= 0) 0
      else m(a*b.head, b.tail:_*)
    }

    def points(cond: List[(BigInt, BigInt, BigInt, BigInt, BigInt)]) = {
      val p = (part zip cond).map {
        case (mult, (cap, dur, fla, tex, cal)) => (mult * cap, mult * dur, mult * fla, mult * tex, mult * cal)
      }
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4, a._5 + b._5))
      (m(p._1, p._2, p._3, p._4), p._5)
    }

    data.map(points).filter(n => calFun(n._2)).map(_._1).foldLeft(BigInt(0))(max)
  }

  def partitionInto(number: Int, groups: Int) = {
    def partitionInto(sum: Int, curr: List[BigInt], acc: Set[List[BigInt]]): Set[List[BigInt]] = {
       if (curr.size == groups - 1) acc + (curr :+ BigInt(number - sum)).sorted
       else (1 to number - sum - (groups - curr.size)).map(n => partitionInto(sum + n, curr :+ BigInt(n), acc)).reduce(_ ++ _)
    }

    partitionInto(0, Nil, Set())
  }
}
