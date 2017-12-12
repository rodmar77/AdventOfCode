import scala.io.Source

object Day09 {

  def main(args: Array[String]): Unit = {
    val line = Source
      .fromFile("inputs/2017/input_day09.txt")
      .mkString
      .trim

    println(getScore(line))
    println(getGarbageCount(line))
  }

  def getScore(line: String): Int = {
    def getScore(ll: String, inGarbage: Boolean, openGroups: Int, acc: Int, idx: Int): Int = {
      if (ll.isEmpty) acc
      else if (ll.head == '!') getScore(ll.drop(2), inGarbage, openGroups, acc, idx + 1)
      else if (ll.head == '>') getScore(ll.tail, false, openGroups, acc, idx + 1)
      else if (ll.head == '<') getScore(ll.tail, true, openGroups, acc, idx + 1)
      else if (inGarbage)      getScore(ll.tail, inGarbage, openGroups, acc, idx + 1)
      else if (ll.head == '}') getScore(ll.tail, inGarbage, openGroups - 1, acc + openGroups, idx + 1)
      else if (ll.head == '{') getScore(ll.tail, inGarbage, openGroups + 1, acc, idx + 1)
      else if (ll.head == ',') getScore(ll.tail, inGarbage, openGroups, acc, idx + 1)
      else throw new IllegalStateException(s"Don't know what to do: at $idx, ll.head == '${ll.head}', inGarbage == $inGarbage, openGroups == $openGroups, acc == $acc")
    }

    getScore(line, false, 0, 0, 0)
  }

  def getGarbageCount(line: String): Int = {
    def getGarbageCount(ll: String, inGarbage: Boolean, acc: Int): Int = {
      if (ll.isEmpty) acc
      else if (ll.head == '!') getGarbageCount(ll.drop(2), inGarbage, acc)
      else if (ll.head == '>') getGarbageCount(ll.tail, false, acc)
      else if (inGarbage)      getGarbageCount(ll.tail, inGarbage, acc + 1)
      else if (ll.head == '<') getGarbageCount(ll.tail, true, acc)
      else getGarbageCount(ll.tail, inGarbage, acc)
    }

    getGarbageCount(line, false, 0)
  }
}
