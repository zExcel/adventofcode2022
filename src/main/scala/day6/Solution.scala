package day6

import util.Control.using
import util.GenericSolution

import scala.collection.mutable
import scala.io.Source

class Solution extends GenericSolution {
  
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
