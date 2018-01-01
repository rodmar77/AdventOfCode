object Day17 {

  def main(args: Array[String]): Unit = {
    val key = 301
    val array = getArray(key, 2018)
    println(array(array.indexOf(2017) + 1))

    val array2 = getArray(key, 50000001)
    println(array2(array2.indexOf(0) + 1))
  }

  def getArray(key: Int, total: Int) = {
    def addToArray(array: List[Int], v: Int, idx: Int): List[Int] = {
      val split = array.splitAt(idx)
      (split._1 :+ v) ++ split._2
    }

    def buildArray(idx: Int, v: Int, a: List[Int]): List[Int] = {
      if (v % 1000 == 0) println(v)
      val nextIdx = (idx + key + 1) % a.size
      if (v == total) a
      else buildArray(nextIdx, v + 1, addToArray(a, v, nextIdx))
    }

    buildArray(1, 1, List(0))
  }

}
