object Day19 {

  def main(args: Array[String]): Unit = {
    println(josephus(3014603))
    println(josephus2(3014603))
  }

  def josephus(total: Int): Int = {
    if (total == 1) 1
    else if (total % 2 == 0) 2 * josephus(total/2) - 1
    else 2 * josephus((total - 1)/2) + 1
  }

  def josephus2(total: Int): Int = {
    def _josephus2(acc: Int): Int = {
      if (acc * 3 < total) _josephus2(acc * 3)
      else total - acc
    }

    _josephus2(1)
  }
}
