object Day10 extends App {

  println(lookAndSay("1321131112", 60).length)

  def lookAndSay(start: String, count: Int) = {
    val regex = """(\d)(\1*)""".r
    def lookAndSay(idx: Int, acc: String): String = {
      if (idx == count) (acc)
      else lookAndSay(
        idx + 1,
        regex
          .findAllMatchIn(acc)
          .map(m => m.group(0).length + m.group(1)).mkString)
    }

    lookAndSay(0, start)
  }

}