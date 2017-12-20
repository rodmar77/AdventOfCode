object Day16 {

  def main(args: Array[String]): Unit = {
    println(checkSum(fill("11101000110010100", 272)))
    println(checkSum(fill("11101000110010100", 35651584)))
  }


  def fill(text: String, size: Int) = {
    def fill (ct: String): String = {
      if (ct.lengthCompare(size) >= 0) ct.take(size)
      else fill(ct + 0 + ct.reverse.map(c => if (c == '1') '0' else '1'))
    }

    fill(text)
  }

  def checkSum(text: String) = {
    def _checkSum(ct: String): String = {
      val v = ct.sliding(2, 2).map(pair => if (pair(0) == pair(1)) '1' else '0').mkString
      if (v.length % 2 == 0) _checkSum(v) else v
    }

    _checkSum(text)
  }
}
