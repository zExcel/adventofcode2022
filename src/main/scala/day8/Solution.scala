package day8

import util.GenericSolution

import scala.collection.immutable.TreeSet

class Grid(val cells: Array[Array[Int]]) {
  
  def isInsideGrid(row: Int, col: Int): Boolean = row >= 0 && row < cells.length && col >= 0 && col < cells(0).length
  
  // information._1 is the largest tree seen so far
  // information._2 is the set of cells that are visible from this direction
  // cell._1 is the tree's height
  // cell._2 is the column number
  def getNewTrackingSet(information: (Int, Set[(Int, Int)]),
                        cell: (Int, Int),
                        newEntry: (Int, Int)): (Int, Set[(Int, Int)]) = {
    if (cell._1 > information._1) {
      (cell._1, information._2 + newEntry)
    } else {
      (information._1, information._2)
    }
  }
  
  def getNumberOfVisibleCells: Int = {
    val visibleCells: Set[(Int, Int)] = {
      // Left to right sweep
      val ltrVisibleCells: Set[(Int, Int)] = cells.zipWithIndex.foldLeft(Set[(Int, Int)]())((trackingSet, row) => {
        row._1.zipWithIndex.foldLeft((-1, trackingSet))((information, cell) => {
          getNewTrackingSet(information, cell, (cell._2, row._2))
        })._2
      })

      val rtlVisibleCells: Set[(Int, Int)] = cells.zipWithIndex.foldLeft(Set[(Int, Int)]())((trackingSet, row) => {
        row._1.reverseIterator.zipWithIndex.foldLeft((-1, trackingSet))((information, cell) => {
          getNewTrackingSet(information, cell, (cells(0).length - cell._2 - 1, row._2))
        })._2
      })

      val ttbVisibleCells: Set[(Int, Int)] = cells.transpose.zipWithIndex.foldLeft(Set[(Int, Int)]())((trackingSet, row) => {
        row._1.zipWithIndex.foldLeft((-1, trackingSet))((information, cell) => {
          getNewTrackingSet(information, cell, (row._2, cell._2))
        })._2
      })

      val bttVisibleCells: Set[(Int, Int)] = cells.transpose.zipWithIndex.foldLeft(Set[(Int, Int)]())((trackingSet, row) => {
        row._1.reverseIterator.zipWithIndex.foldLeft((-1, trackingSet))((information, cell) => {
          getNewTrackingSet(information, cell, (row._2, cells.length - cell._2 - 1))
        })._2
      })

      ltrVisibleCells ++ rtlVisibleCells ++ ttbVisibleCells ++ bttVisibleCells
    }
    
    visibleCells.size
  }

//  private class TreeCell(val height: Int, val position: (Int, Int)) {
//    
//  }


//  def updateScenicMap()
  
//  def addTreeToSet(treeHeight: Int,
//                   position: (Int, Int),
//                   set: TreeSet[((Int, Int), Int)],
//                   scenicMap: Map[(Int, Int), Int]): (TreeSet[Int], Map[(Int, Int), Int]) = {
//    val treesToUpdate = set.filter(_._2 <= treeHeight)
//    val newSet = set.removedAll(treesToUpdate) + (position, treeHeight)
//    ()
//    
//  } 
  
  // Idea here is to have an ordered Set with the values of (tree_height, position) where the value of an 
  // item is its height. As we traverse (in any direction) we add that tree cell to the set. Any tree cells that are 
  // LESS THAN the cell that was just added is then removed and added to a Map [with values of (position, visible_trees)]
  // with the number of trees traversed since it was added (calculated from the current position and the tree's position).
//  def getOptimalScenicScore: Int = {
//    
//  }
}

object Grid {
  def apply(stringCells: Array[String]): Grid = new Grid(stringCells.foldLeft(Array[Array[Int]]())((array, line) => {
    array :+ line.map(char => char.toInt - '0'.toInt).toArray
  }))
}

class Solution extends GenericSolution {
  
  def solvePartOne(input: Array[String]): Int = {
    val grid: Grid = Grid(input)
    grid.getNumberOfVisibleCells
  }

  def solvePartTwo(input: Array[String]): Int = {
    5
  }
}
