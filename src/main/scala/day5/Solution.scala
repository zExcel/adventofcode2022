package day5

import util.Control.using
import util.GenericSolution

import scala.collection.mutable
import scala.io.Source

class Instruction(val amount: Int, val from: Int, val to: Int) {

  // Assumed to be in the format of [2, 4, 2]
  def this(instructionArray: Array[String]) = {
    this(instructionArray(0).toInt, instructionArray(1).toInt, instructionArray(2).toInt)
  }

  // Assumed to be in the format of "move 2 from 4 to 2"
  def this(stringInstruction: String) = {
    this(stringInstruction.replaceAll("[a-zA-Z]+", "").trim().split("[ ]+").map(_.trim()))
  }

}

class Solution extends GenericSolution {

  override def readFile(filePath: String): Array[String] = {
    using(Source.fromResource(filePath)) {
      source => {
        source.mkString.split("\n\n")
      }
    }
  }

  // This kinda assumes that the character piles will be directly in line with the numbered column (like below)
  //    [G]         [P]         [M]
  //[C] [H]     [T] [T] [G] [B] [Z] [B]
  //[B] [T] [M] [B] [J] [C] [T] [G] [N]
  // 1   2   3   4   5   6   7   8   9
  def parseStack(stack: String): Array[mutable.Stack[Char]] = {
    val lines = stack.split("\n")
    val pileLine = lines(lines.length - 1)
    val stackLines = lines.dropRight(1).reverse
    val piles = pileLine.trim().split("[ ]+").map(_.trim())
    val stacks = Array.fill[mutable.Stack[Char]](piles.length)(mutable.Stack())

    piles.foreach(pileNumber => {
      val pileNumberValue = pileNumber.toInt
      val position = pileLine.indexOf(pileNumber)
      stackLines.foreach(stackLine => {
        val char = stackLine.charAt(position)
        if (char != ' ') stacks(pileNumberValue - 1).push(char)
      })
    })
    stacks
  }

  def doInstruction(instruction: Instruction, stacks: Array[mutable.Stack[Char]]): Unit = {
    Range(0, instruction.amount).foreach(number => {
      val char = stacks(instruction.from - 1).pop()
      stacks(instruction.to - 1).push(char)
    })
  }

  def doInstructionPartTwo(instruction: Instruction, stacks: Array[mutable.Stack[Char]]): Unit = {
    val tempStack: mutable.Stack[Char] = mutable.Stack()
    Range(0, instruction.amount).foreach(number => {
      val char = stacks(instruction.from - 1).pop()
      tempStack.push(char)
    })
    stacks(instruction.to - 1).pushAll(tempStack)
  }

  def solvePartOne(input: Array[String]): String = {
    val stacks = parseStack(input(0))
    val instructions = input(1).split("\n").map(new Instruction(_))
    instructions.foreach(doInstruction(_, stacks))
    stacks.map(_.pop()).mkString("")
  }

  def solvePartTwo(input: Array[String]): String = {
    val stacks = parseStack(input(0))
    val instructions = input(1).split("\n").map(new Instruction(_))
    instructions.foreach(doInstructionPartTwo(_, stacks))
    stacks.map(_.pop()).mkString("")
  }
}
