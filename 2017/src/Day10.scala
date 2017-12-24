import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    val data = Source
      .fromFile("inputs/2017/input_day10.txt")
      .getLines
      .mkString

    val (spaces, spacesFromBytes) = (
      data
        .split(",")
        .map(_.toInt)
        .toList,

      data
        .getBytes("ASCII")
        .toList
        .map(_.toInt)
        ++ List(17, 31, 73, 47, 23))

    println(
      applyRotations(spaces)
        .take(2)
        .product)

    println(
      applyRotations(spacesFromBytes, 64)
        .sliding(16, 16)
        .map(_.reduce(_ ^ _))
        .map("%02x".format(_))
        .mkString)
  }

  implicit class x[A](as: List[A]) {
    def updatedAt(is: List[Int], f: (Int, List[Int]) => A) = {
      (as /: is.indices) {
        case (xx, i) => xx updated(is(i) % as.size, f(i, is))
      }
    }

    def copy = as.indices.map(as(_))
  }

  def applyRotations(groups: List[Int], rounds: Int = 1) = {
    def applyRotation(idx: Int, ridx: Int, skipSize: Int, grps: List[Int], array: List[Int]): List[Int] = {
      if (ridx == rounds) array

      else if (grps.isEmpty) applyRotation(
        idx,
        ridx + 1,
        skipSize,
        groups,
        array)

      else applyRotation(
        (idx + skipSize + grps.head) % array.size,
        ridx,
        skipSize + 1,
        grps.tail,
        array.updatedAt(
          (idx until idx + grps.head).toList,
          (i, is) => {
            array(is(is.size - i - 1) % array.length)
          }))
    }

    applyRotation(0, 0, 0, groups, (0 to 255).toList)
  }
}
