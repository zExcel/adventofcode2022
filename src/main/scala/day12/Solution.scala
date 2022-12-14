package day12

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.write
import util.GenericSolution

class Point(val row: Int, val col: Int) {

  implicit val formats: DefaultFormats.type = DefaultFormats

  override def toString: String = write(this)
  
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Point]) return false
    val objPoint = obj.asInstanceOf[Point]
    row == objPoint.row && col == objPoint.col
  }

  override def hashCode(): Int = row * 1237569 + col
}

object Point {
  def apply(currentPos: Point, directionTravel: Point): Point = 
    new Point(currentPos.row + directionTravel.row, currentPos.col + directionTravel.col)
}

class Grid(val grid: Array[Array[Char]],
           val start: Point,
           val end: Point) {

  val impossiblePositionChar: Char = ' '
  val impossibleTransitionPoint: Point = new Point(-1, -1)
  
  val directions: List[Point] = List(new Point(-1, 0),
    new Point(1, 0),
    new Point(0, -1),
    new Point(0, 1))
  
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def toString: String = write(this)

  def getPoint(position: Point): Char = {
    if (position.row < 0 || position.row >= grid.length || position.col < 0 || position.col >= grid(0).length)
      return impossiblePositionChar
    val char = grid(position.row)(position.col)
    if (char == 'S') return 'a'
    if (char == 'E') return 'z'
    char
  }
  
  def isPossibleTransition(posFrom: Point, posTo: Point): Boolean = {
    val charFrom = getPoint(posFrom)
    val charTo = getPoint(posTo)
    charTo != impossiblePositionChar && charTo.toInt - 1 <= charFrom.toInt
  }
  
  def findPossibleDirections(currentPos: Point): List[Point] = {
    directions.map(direction => {
      val newPoint = Point(currentPos, direction)
      if (isPossibleTransition(currentPos, newPoint)) newPoint else impossibleTransitionPoint
    }).filterNot(point => point.equals(impossibleTransitionPoint)) // Gets all non-impossible transition points
  }

  def findShortestPath(visited: scala.collection.mutable.Map[Point, Int] = scala.collection.mutable.Map(),
                       position: Point = start,
                       distance: Int = 0): Int = {
    if (visited.contains(position) && visited(position) <= distance) return Integer.MAX_VALUE
    if (position.equals(end)) return distance
    visited.addOne(position -> distance)
    val possibleDirections = findPossibleDirections(position)
    possibleDirections.map(findShortestPath(visited, _, distance + 1)).min
  }
}

object Grid {

  def lookForCharacter(input: Array[String], char: Char): Point =
    (for (col <- input(0).indices;
          row <- input.indices
          if input(row)(col) == char) yield new Point(row, col)).apply(0)


  def apply(input: Array[String]): Grid = {
    val startingPosition = lookForCharacter(input, 'S')
    val endingPosition = lookForCharacter(input, 'E')
    new Grid(input.map(_.toCharArray), startingPosition, endingPosition)
  }
}

class Solution extends GenericSolution {
  def solvePartOne(input: Array[String]): Int = {
    val grid = Grid(input)
    grid.findShortestPath()
  }

  def solvePartTwo(input: Array[String]): Int = {
    val grid = Grid(input)
    val visitedMap = scala.collection.mutable.Map[Point, Int]()
    val firstPath = grid.findShortestPath(visited = visitedMap)
    val optimalPath = (for (col <- input(0).indices;
          row <- input.indices
          if input(row)(col) == 'a') yield new Point(row, col)).map(startingPoint => {
      grid.findShortestPath(visitedMap, startingPoint)
    }).min
    Math.min(firstPath, optimalPath)
  }
}