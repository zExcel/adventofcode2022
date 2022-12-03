package day1

import scala.io.Source
import util.Control.using
import util.GenericSolution

class Solution extends GenericSolution{

  override def readFile(filePath: String): Array[String] = {
    using(Source.fromResource(filePath)) {
      source => {
        source.mkString.split("\n\n")
      }
    }
  }

  def solvePartOne(input: Array[String]): Int = {
    val sums: Array[Int] = input.map(strings => strings.split("\n")).map(array => {
      array.map(value => value.toInt).sum
    })

    sums.max
  }

  def solvePartTwo(input: Array[String]): Int = {
    val sums: Array[Int] = input.map(strings => strings.split("\n")).map(array => {
      array.map(value => value.toInt).sum
    })

    sums.sorted.drop(sums.length - 3).sum
  }
}
