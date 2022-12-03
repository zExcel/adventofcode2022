package day3

import util.GenericSolution


class Solution extends GenericSolution {

  private def getCharValue(char: Char): Int = {
    if (char <= 'Z') {
      27 + char.toInt - 'A'.toInt
    } else  {
      1 + char.toInt - 'a'.toInt
    }
  }

  def solvePartOne(input: Array[String]): Int = {
    input.map(line => {
      val (firstSack, secondSack) = line.splitAt(line.length/2)
      val firstSackSet = firstSack.toSet
      secondSack.map(char => if (firstSackSet.contains(char)) getCharValue(char) else 0).distinct.filterNot(x => x == 0).sum
    }).sum
  }

  def solvePartTwo(input: Array[String]): Int = {
    input.grouped(3).toList.map(group => {
      val firstSackSet = group(0).toSet
      val secondSackSet = group(1).map(char => if (firstSackSet.contains(char)) char else ' ').distinct.filterNot(x => x == ' ')
      group(2).map(char => if (secondSackSet.contains(char)) getCharValue(char) else 0).distinct.filterNot(x => x == 0).sum
    }).sum
  }
}
