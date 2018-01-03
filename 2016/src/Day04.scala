import scala.io.Source

/*
  --- Day 4: Security Through Obscurity ---

  Finally, you come across an information kiosk with a list of rooms. Of course,
  the list is encrypted and full of decoy data, but the instructions to decode
  the list are barely hidden nearby. Better remove the decoy data first.

  Each room consists of an encrypted name (lowercase letters separated by dashes)
  followed by a dash, a sector ID, and a checksum in square brackets.

  A room is real (not a decoy) if the checksum is the five most common letters in
  the encrypted name, in order, with ties broken by alphabetization. For example:

      * aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters
        are a (5), b (3), and then a tie between x, y, and z, which are listed
        alphabetically.

      * a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters
        are all tied (1 of each), the first five are listed alphabetically.

      * not-a-real-room-404[oarel] is a real room.

      * totally-real-room-200[decoy] is not.

  Of the real rooms from the list above, the sum of their sector IDs is 1514.

 */
object Day04 {

  private val idPattern = ".+?-([0-9]+)\\[.+\\]".r
  private val dataPattern = "(.+?)-[0-9]+\\[(.+)\\]".r

  def main(args: Array[String]): Unit = {
    val ids = Source
      .fromFile("inputs/2016/input_day04.txt")
      .getLines
      .toList

    // What is the sum of the sector IDs of the real rooms?
    println(ids.filter(isValidRoom).map(extractId).sum)

    /*
    With all the decoy data out of the way, it's time to decrypt this list and
    get moving.

    The room names are encrypted by a state-of-the-art shift cipher, which is
    nearly unbreakable without the right software. However, the information
    kiosk designers at Easter Bunny HQ were not expecting to deal with a master
    cryptographer like yourself.

    To decrypt a room name, rotate each letter forward through the alphabet a
    number of times equal to the room's sector ID. A becomes B, B becomes C, Z
    becomes A, and so on. Dashes become spaces.

    For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted
    name.

    What is the sector ID of the room where North Pole objects are stored?
     */
    println(ids.find(rotate(_).startsWith("NORTHPOLE")) match {
      case Some(room) => extractId(room)
    })
  }

  def extractId(s: String) = idPattern.findFirstMatchIn(s).get.group(1).toInt

  def isValidRoom(s: String): Boolean = {
    def frequency(t: String) = t
      .replace("-", "")
      .groupBy(_.toChar)
      .map {
        case (ch, g)  => (ch, g.length)
      }
      .toList

    def expected(t: String) = frequency(t)
      .sortBy(a => (-a._2, a._1))
      .map(_._1)
      .take(5)
      .mkString

    s match {
      case dataPattern(letters, actual) => expected(letters).equals(actual)
    }
  }

  def rotate(s: String): String = {
    val (alphabet, dataPattern(letters, _), idPattern(id)) = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", s, s)
    letters.map(c => {
      if (c == '-') c
      else (alphabet.indexOf(c.toUpper), id.toInt) match {
        case (idx, shift) => alphabet((idx + shift) % alphabet.length)
      }
    })
  }
}
