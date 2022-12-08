package day6

import util.GenericSolution

class Solution extends GenericSolution {
  
  // God I love Scala
  def findPacketStartMarker(packet: String, windowSize: Int): Int = {
    packet.indexOf(packet.sliding(windowSize).find(_.distinct.length == windowSize).get) + windowSize
  }

  def solvePartOne(input: Array[String]): Int = {
    findPacketStartMarker(input(0), 4)
  }

  def solvePartTwo(input: Array[String]): Int = {
    findPacketStartMarker(input(0), 14)
  }
}
