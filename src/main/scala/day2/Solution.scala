package day2

import util.GenericSolution

class Solution extends GenericSolution {

  private def selectionValuer(character: Char): Int = {
    val values: Map[Char, Int] = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)
    values(character)
  }

  private def scorer(opponent: Char, mine: Char): Int = {
    if (opponent == 'A') {
      if (mine == 'X') {
        3 + selectionValuer(mine)
      } else if (mine == 'Y' ) {
        6 + selectionValuer(mine)
      } else {
        selectionValuer(mine)
      }
    } else if (opponent == 'B') {
      if (mine == 'X') {
        selectionValuer(mine)
      } else if (mine == 'Y') {
        3 + selectionValuer(mine)
      } else {
        6 + selectionValuer(mine)
      }
    } else {
      if (mine == 'X') {
        6 + selectionValuer(mine)
      } else if (mine == 'Y') {
        selectionValuer(mine)
      } else {
        3 + selectionValuer(mine)
      }
    }
  }

  def solvePartOne(input: Array[String]): Int = {
    input.map(game => {
      val choices = game.trim.split(" ")
      scorer(choices(0)(0), choices(1)(0))
    }).sum
  }

  def solvePartTwo(input: Array[String]): Int = {
    val myChoiceSelector: Map[(Char, Char), Char] = Map(
      ('A', 'X') -> 'Z',
      ('A', 'Y') -> 'X',
      ('A', 'Z') -> 'Y',
      ('B', 'X') -> 'X',
      ('B', 'Y') -> 'Y',
      ('B', 'Z') -> 'Z',
      ('C', 'X') -> 'Y',
      ('C', 'Y') -> 'Z',
      ('C', 'Z') -> 'X',
    )
    input.map(game => {
      val choices = game.trim.split(" ")
      scorer(choices(0)(0), myChoiceSelector((choices(0)(0), choices(1)(0))))
    }).sum
  }
}
