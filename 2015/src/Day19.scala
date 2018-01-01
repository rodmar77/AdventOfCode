import scala.io.Source

/*

  --- Day 19: Medicine for Rudolph ---

  Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
  and he needs medicine.

  Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
  is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer
  chemistry isn't similar to regular reindeer chemistry, either.

  The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission
  plant, capable of constructing any Red-Nosed Reindeer molecule you need. It
  works by starting with some input molecule and then doing a series of replacements,
  one per step, until it has the right molecule.

  However, the machine has to be calibrated before it can be used. Calibration
  involves determining the number of molecules that can be generated in one step
  from a given starting point.

  For example, imagine a simpler machine that supports only the following replacements:

  +---------+
  | H => HO |
  | H => OH |
  | O => HH |
  +---------+

  Given the replacements above and starting with HOH, the following molecules
   could be generated:

  * HOOH (via H => HO on the first H).
  * HOHO (via H => HO on the second H).
  * OHOH (via H => OH on the first H).
  * HOOH (via H => OH on the second H).
  * HHHH (via O => HH).

  So, in the example above, there are 4 distinct molecules (not five, because HOOH
  appears twice) after one replacement from HOH. Santa's favorite molecule, HOHOHO,
  can become 7 distinct molecules (over nine replacements: six from H, and three
  from O).

  The machine replaces without regard for the surrounding characters. For example,
  given the string H2O, the transition H => OO would result in OO2O.

 */
object Day19 extends App {

  implicit def stringToString(s: String) = new StringWithReplaceAt(s)

  class StringWithReplaceAt(value: String) {
    def cutAndAdd(data: String, pos: Int, size: Int) = value.substring(0, pos) + data + value.substring(pos + size)
    def countStr(s: String) = {
      def count(i: Int, acc: Int): Int = {
        val nextIndex = value.indexOf(s, i + 1)
        if (nextIndex < 0) acc
        else count(nextIndex + s.length - 1, acc + 1)
      }

      count(-1, 0)
    }
  }

  val regex = "(.+?) => (.+?)".r
  val data = Source
    .fromFile("inputs/2015/input_day19.txt")
    .getLines
    .toList

  val replacements = data.takeWhile(!_.isEmpty).map { case regex(from, to) => (from, to) }

  // Your puzzle input describes all of the possible replacements and, at the bottom,
  // the medicine molecule for which you need to calibrate the machine. How many
  // distinct molecules can be created after all the different ways you can do one
  // replacement on the medicine molecule?
  println(replacements.flatMap(getAllReplacements(data.last, _)).distinct.size)

  /*

  Now that the machine is calibrated, you're ready to begin molecule fabrication.

  Molecule fabrication always begins with just a single electron, e, and applying
  replacements one at a time, just like the ones during calibration.

  For example, suppose you have the following replacements:

  +---------+
  | e => H  |
  | e => O  |
  | H => HO |
  | H => OH |
  | O => HH |
  +---------+

  If you'd like to make HOH, you start with e, and then make the following
  replacements:

  e => O to get O
  O => HH to get HH
  H => OH (on the second H) to get HOH

  So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can
  be made in 6 steps.

  How long will it take to make the medicine? Given the available replacements
  and the medicine molecule in your puzzle input, what is the fewest number of
  steps to go from e to the medicine molecule?

   */
  println(getMinReplacementCount(data.last))

  def getAllReplacements(s: String, r: (String, String)): List[String] = {
    def getAllReplacements(curr: Int, acc: List[String]): List[String] = {
      if (curr < 0) acc
      else getAllReplacements(
        s.indexOf(r._1, curr + 1),
        acc :+ s.cutAndAdd(r._2, curr, r._1.length))
    }

    getAllReplacements(s.indexOf(r._1), Nil)
  }

  /*

  All of the rules are of one of the following forms:

  -> x => 1|2
  -> x => 1|Rn|2|Ar
  -> x => 1|Rn|2|Y|3|Ar
  -> x => 1|Rn|2|Y|3|Y|4|Ar

  As Rn, Ar, and Y are only on the left side of the equation, one merely only needs to compute

  #NumSymbols - #Rn - #Ar - 2 * #Y - 1
  Subtract of #Rn and #Ar because those are just extras. Subtract two times #Y because we get rid of
  the Ys and the extra elements following them. Subtract one because we start with "e".

  */
  def getMinReplacementCount(str: String) = {
    str.count(_.isUpper) - str.countStr("Rn") - str.countStr("Ar") - 2 * str.countStr("Y") - 1
  }

}
