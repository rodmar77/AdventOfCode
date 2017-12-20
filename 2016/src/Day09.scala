import scala.io.Source

object Day09 {

  def main(args: Array[String]): Unit = {
    val compressed = Source.fromFile("inputs/2016/input_day09.txt").mkString
    println(decompressedLength(compressed))
    println(decompressedLengthWithExpand(compressed))
  }

  def decompressedLength(compressed: String) = {
    def decompress(ct: String, acc: Int): Int = {
      val end = ct.indexOf(')')
      val lengths = ct.substring(1, end).split("x").map(_.toInt)
      decompressedLength(ct.drop(end + 1 + lengths(0)), acc + lengths(0) * lengths(1))
    }

    def decompressedLength(ct: String, acc: Int): Int = {
      if (ct.isEmpty) acc
      else if (ct.head == '(') decompress(ct, acc)
      else decompressedLength(ct.tail, acc + 1)
    }

    decompressedLength(compressed, 0)
  }

  def decompressedLengthWithExpand(compressed: String) = {
    def decompress(ct: String): BigInt = {
      val end = ct.indexOf(')')
      val lengths = ct.substring(1, end).split("x").map(_.toInt)

      BigInt(lengths(1)) * decompressedLength(ct.substring(end + 1, lengths(0) + end + 1)) +
        decompressedLength(ct.drop(lengths(0) + end + 1))
    }

    def decompressedLength(ct: String): BigInt = {
      if (ct.isEmpty) 0
      else if (ct.head == '(') decompress(ct)
      else if (ct.contains('(')) {
        val index = ct.indexOf('(')
        index + decompressedLength(ct.substring(index))
      } else ct.length
    }

    decompressedLength(compressed)
  }
}
