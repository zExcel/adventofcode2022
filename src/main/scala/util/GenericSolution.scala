package util

import util.Control.using

import scala.io.Source

trait GenericSolution {

  def solvePartOne(input: Array[String]): Any

  def solvePartTwo(input: Array[String]): Any

  def readFile(filePath: String): Array[String] = {
    using(Source.fromResource(filePath)) {
      source => {
        source.mkString.split("\n")
      }
    }
  }

  def solve(input: Array[String]): Unit = {
    val partOneSolution = solvePartOne(input)
    val partTwoSolution = solvePartTwo(input)
    println(s"Part one Solution: $partOneSolution")
    println(s"Part two Solution: $partTwoSolution")
  }
}
