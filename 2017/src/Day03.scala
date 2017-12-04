object Day03 {

  def main(args: Array[String]): Unit = {
    println(dist((0, 0), pos(368078)))
    println(generateAndFind(9, 368078))
  }

  def dist(a: (Int, Int), b: (Int, Int)): Int = (a._1 - b._1).abs + (a._2 - b._2).abs

  def pos(n: Int) = {
    val k = ((Math.sqrt(n) - 1) / 2).ceil.toInt
    val t = 2 * k + 1

    def pos(m: Int, t: Int): (Int, Int) = {
      if (n >= m - t) (k - (m - n), -k)
      else if (n >= m - 2 * t) (-k, -k + (m - t - n))
      else if (n >= m - 3 * t) (-k + (m - 2 * t - n), k)
      else (k, k - (m - n - 3 * t))
    }

    pos(t * t, t - 1)
  }

  def generateAndFind(w: Int, target: Int): Int = {
    abstract class Position(p: (Int, Int), m: List[List[Int]]) {
      def move: Position
      def updatedValue: Int = updated(p._2)(p._1)

      protected def updated: List[List[Int]] = m.updated(p._2, m(p._2).updated(p._1, getValue))
      protected def getValue: Int = (-1 to 1).flatMap(y => (-1 to 1).map(x => valueAt(p._1 + x, p._2 + y))).sum - m(p._2)(p._1)
      protected def canMove(x: Int, y: Int): Boolean = isDefinedAt(x, y) && valueAt(x, y) == 0

      private def isDefinedAt(x: Int, y: Int) = m.isDefinedAt(y) && m(y).isDefinedAt(x)
      private def valueAt(x: Int, y: Int) = if (isDefinedAt(x, y)) m(y)(x) else 0
    }

    case class Right(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1, p._2 - 1)) Up((p._1, p._2 - 1), updated)
      else Right((p._1 + 1, p._2), updated)
    }

    case class Left(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1, p._2 + 1)) Down((p._1, p._2 + 1), updated)
      else Left((p._1 - 1, p._2), updated)
    }

    case class Up(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1 - 1, p._2)) Left((p._1 - 1, p._2), updated)
      else Up((p._1, p._2 - 1), updated)
    }

    case class Down(p: (Int, Int), m: List[List[Int]]) extends Position(p, m) {
      def move: Position = if (canMove(p._1 + 1, p._2)) Right((p._1 + 1, p._2), updated)
      else Down((p._1, p._2 + 1), updated)
    }

    def generate(c: Int, p: Position): Int = {
      if (p.updatedValue > target) p.updatedValue
      else generate(c + 1, p.move)
    }

    val (mi, acc) = ((w - 1) / 2, List.fill(w, w)(0))
    generate(1, Right((mi + 1, mi), acc.updated(mi, acc(mi).updated(mi, 1))))
  }
}
