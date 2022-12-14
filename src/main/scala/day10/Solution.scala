package day10

import util.GenericSolution

import scala.annotation.tailrec

class Instruction(val stepsToTake: Int, val valueToAdd: Int, var stepStarted: Int) {
  override def toString: String = s"{stepsToTake: $stepsToTake, valueToAdd: $valueToAdd, stepStarted: $stepStarted}"
}

class Solution extends GenericSolution {
  
  @tailrec
  private def processInstructions(registerValue: Int,
                                  instructions: List[Instruction],
                                  interestingSignals: Int,
                                  currentStep: Int,
                                  imageRender: String): (Int, String) = {
    val currentInstruction = instructions.head
    if (currentInstruction.stepStarted == -1) currentInstruction.stepStarted = currentStep
    val shouldAddValue = currentStep - currentInstruction.stepStarted == currentInstruction.stepsToTake - 1
    val newInstructions = if (shouldAddValue) instructions.tail else instructions
    val valueToAdd = if (shouldAddValue) currentInstruction.valueToAdd else 0
    val newRegister = registerValue + valueToAdd
    val newSignalStrength = if (currentStep % 40 == 20) (interestingSignals + currentStep * registerValue) else interestingSignals
    val charToDraw = if (Math.abs(registerValue - ((currentStep- 1) % 40)) <= 1) '#' else '.'
    if (newInstructions.isEmpty) {
      (interestingSignals, imageRender)
    } 
    else {
      processInstructions(newRegister, newInstructions, newSignalStrength, currentStep + 1, imageRender + charToDraw)
    }
  }
  
  def constructInstructionsList(input: Array[String]): List[Instruction] = {
    input.foldLeft(List[Instruction]())((list, currentInstruction) => {
      val instructionToAdd = if (currentInstruction.contains("noop")) {
        new Instruction(1, 0, -1)
      } else {
        new Instruction(2, currentInstruction.split(" ").apply(1).toInt, -1)
      }
      list :+ instructionToAdd
    })
  }
  
  def solvePartOne(input: Array[String]): Int = {
    val instructionList = constructInstructionsList(input)
    
    processInstructions(1, instructionList, 0, 1, "")._1
  }

  def solvePartTwo(input: Array[String]): Int = {
    val instructionList = constructInstructionsList(input)

    val resultingImage = processInstructions(1, instructionList, 0, 1, "")._2
    println("vvv Part two solution is actually this image vvv")
    resultingImage.grouped(40).toList.foreach(println(_))
    println("^^^ Part two solution is actually this image ^^^")
    5
  }
}