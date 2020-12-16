import scala.io.Source
import scala.util.Using

/*
  --- Day 6: Passport Processing ---
 */

object Day06 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2020/input_day06.txt")) {
      source => val data = getData(source
        .getLines
        .toList)

        println(data.map(_.reduce(_ + _).distinct.length).sum)

        /*
          --- Part Two ---

          As you finish the last group's customs declaration, you notice that
          you misread one word in the instructions:

          You don't need to identify the questions to which anyone answered "yes";
          you need to identify the questions to which everyone answered "yes"!

          Using the same example as above:

            abc

            a
            b
            c

            ab
            ac

            a
            a
            a
            a

            b

          This list represents answers from five groups:

            In the first group, everyone (all 1 person) answered "yes" to 3
            questions: a, b, and c.

            In the second group, there is no question to which everyone
            answered "yes".

            In the third group, everyone answered yes to only 1 question, a.
            Since some people did not answer "yes" to b or c, they don't count.

            In the fourth group, everyone answered yes to only 1 question, a.
            In the fifth group, everyone (all 1 person) answered "yes" to 1
            question, b.

            In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.

          For each group, count the number of questions to which everyone
          answered "yes". What is the sum of those counts?
        */

        println(data.map(countYesAnswers).sum)
    }
  }

  def countYesAnswers(ll: List[String]): Int = ll.reduce(_ + _).distinct.count(c => ll.forall(_.contains(c)))

  def getData(ll: List[String]): List[List[String]] = {
    def getData(lines: List[String], acc: List[String], result: List[List[String]]): List[List[String]] = {
      if (lines.isEmpty) result :+ acc
      else if (lines.head.isEmpty) getData(lines.tail, Nil, result :+ acc)
      else getData(lines.tail, acc :+ lines.head, result)
    }

    getData(ll, Nil, Nil)
  }

}
