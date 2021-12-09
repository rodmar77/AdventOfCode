import scala.io.Source
import scala.util.Using

/*
  --- Day 9: Smoke Basin ---

  These caves seem to be lava tubes. Parts are even still volcanically active; small
  hydrothermal vents release smoke into the caves that slowly settles like rain.

  If you can model how the smoke flows through the caves, you might be able to avoid it
  and be that much safer. The submarine generates a heightmap of the floor of the nearby
  caves for you (your puzzle input).

  Smoke flows to the lowest point of the area it's in. For example, consider the
  following heightmap:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

  Each number corresponds to the height of a particular location, where 9 is the highest
  and 0 is the lowest a location can be.

  Your first goal is to find the low points - the locations that are lower than any of
  its adjacent locations. Most locations have four adjacent locations (up, down, left, and
  right); locations on the edge or corner of the map have three or two adjacent locations,
  respectively. (Diagonal locations do not count as adjacent.)

  In the above example, there are four low points, all highlighted: two are in the first
  row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5).
  All other locations on the heightmap have some lower adjacent location, and so are not
  low points.

  The risk level of a low point is 1 plus its height. In the above example, the risk levels
  of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the
  heightmap is therefore 15.

 */
object Day09 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("inputs/2021/input_day09.txt")) {
      source =>
        val matrix = source.getLines().map(_.toCharArray.map(_ - '0').toList).toList

        // Find all of the low points on your heightmap. What is the sum of the risk
        // levels of all low points on your heightmap?
        val lowPoints = matrix.indices.flatMap(line => matrix(line).indices.map(col => (line, col))).filter {
          case (line, col) => isLowPoint(matrix, line, col)
        }

        println(lowPoints.map {
          case (line, col) => matrix(line)(col) + 1
        }.sum)

        /*
          --- Part Two ---

          Next, you need to find the largest basins so you know what areas are most
          important to avoid.

          A basin is all locations that eventually flow downward to a single low point.
          Therefore, every low point has a basin, although some basins are very small.
          Locations of height 9 do not count as being in any basin, and all other
          locations will always be part of exactly one basin.

          The size of a basin is the number of locations within the basin, including the
          low point. The example above has four basins.

          The top-left basin, size 3:

            2199943210
            3987894921
            9856789892
            8767896789
            9899965678

          The top-right basin, size 9:

            2199943210
            3987894921
            9856789892
            8767896789
            9899965678

          The middle basin, size 14:

            2199943210
            3987894921
            9856789892
            8767896789
            9899965678

          The bottom-right basin, size 9:

            2199943210
            3987894921
            9856789892
            8767896789
            9899965678

          Find the three largest basins and multiply their sizes together. In the
          above example, this is 9 * 14 * 9 = 1134.

          What do you get if you multiply together the sizes of the three largest basins?
         */
        val basins = lowPoints.map(getBasin(matrix, _).size)
        println(basins.sortBy(-_).take(3).product)
    }
  }

  def getBasin(matrix: List[List[Int]], position: (Int, Int)): Set[(Int, Int)] = {
    def isValidPosition(position: (Int, Int)): Boolean = position match {
      case (line, col) => line >= 0 && col >= 0 && line < matrix.length && col < matrix(line).length
    }

    def getValidBasinNeighbors(position: (Int, Int)) = position match {
      case (line, col) =>
        List((line-1, col), (line+1, col), (line, col-1), (line, col+1))
        .filter {
          case (x, y) => isValidPosition(x, y) && matrix(x)(y) < 9
        }
    }

    def floodFill(acc: Set[(Int, Int)], current: (Int, Int)): Set[(Int, Int)] = {
      val neighbors = getValidBasinNeighbors(current).filterNot(acc)
      if (neighbors.isEmpty) acc
      else neighbors.foldLeft(acc ++ neighbors)(floodFill)
    }

    floodFill(Set(), position) + position
  }

  def isLowPoint(matrix: List[List[Int]], line: Int, col: Int): Boolean = {
    val current = matrix(line)(col)

    if (line > 0 && matrix(line-1)(col) <= current) false
    else if (line < matrix.length - 1 && matrix(line+1)(col) <= current) false
    else if (col > 0 && matrix(line)(col - 1) <= current) false
    else if (col < matrix(line).length - 1 && matrix(line)(col+1) <= current) false
    else true
  }
}

