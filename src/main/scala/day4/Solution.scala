package day4

import util.GenericSolution

class Section(val lowerSection: Int, val upperSection: Int) {

  def this(sections: Array[String]) = this(sections(0).toInt, sections(1).toInt)

  def isInsideSection(otherSection: Section): Boolean = {
    lowerSection >= otherSection.lowerSection && upperSection <= otherSection.upperSection
  }

  def overlaps(otherSection: Section): Boolean = {
    lowerSection >= otherSection.lowerSection && lowerSection <= otherSection.upperSection ||
      otherSection.lowerSection >= lowerSection && otherSection.lowerSection <= upperSection
  }
}

class Solution extends GenericSolution {

  def solvePartOne(input: Array[String]): Int = {
    input.map(_.split(",")).map(pair => {
      (new Section(pair(0).split("-")), new Section(pair(1).split("-")))
    }).map(pair => pair._1.isInsideSection(pair._2) || pair._2.isInsideSection(pair._1)).count(value => value)
  }

  def solvePartTwo(input: Array[String]): Int = {
    input.map(_.split(",")).map(pair => {
      (new Section(pair(0).split("-")), new Section(pair(1).split("-")))
    }).map(pair => pair._1.overlaps(pair._2)).count(value => value)
  }
}
