package day7

import util.GenericSolution

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class File (val fileName: String, var fileSize: Int, val isDirectory: Boolean, val children: ListBuffer[File], val parent: File) {
  
  def this(fileName: String, isDirectory: Boolean, parent: File) {
    this(fileName, 0, isDirectory, new ListBuffer[File](), parent)
  }
  
  def addFile(otherFile: File): Unit = {
    children.addOne(otherFile)
  }
  
  def getFileSize: Int = {
    fileSize = children.foldLeft(0)((acc, file) => {
      acc + (if (file.isDirectory) file.getFileSize else file.fileSize)
    })
    fileSize
  }
}

class Solution extends GenericSolution {
  val AVAILABLE_SPACE = 70000000
  val DESIRED_FREE_SPACE = 30000000
  
  val CD_FORMAT = "\\$ cd.*"
  val LS_FORMAT = "\\$ ls"
  val DIR_FORMAT = "dir.*"
  
  def createFileStructure(input: Array[String]): File = {
    val fileSystem = new File("/", true, null)
    var currentFile = fileSystem
    input.foldLeft(fileSystem)((currentFile, command) => {
      if (command.matches(CD_FORMAT)) {
        val directory = command.drop(5)
        if (directory == "/") {
          fileSystem
        } else if (directory == "..") {
          currentFile.parent
        } else {
          currentFile.children.find(_.fileName == directory).get
        }
      } else if (command.matches(LS_FORMAT)) {
        currentFile
      } else if (command.matches(DIR_FORMAT)) {
        currentFile.children.addOne(new File(command.drop(4), true, currentFile))
        currentFile
      } else {
        val fileParts = command.split(" ")
        currentFile.children.addOne(new File(fileParts(1), fileParts(0).toInt, false, null, currentFile))
        currentFile
      }
    })
    fileSystem.getFileSize
    fileSystem
  }
  
  private def getAllFilesUnderLimit(currentFile: File, limit: Int): Int = {
    if (currentFile.children == null) {
      return 0
    }
    (if (currentFile.fileSize <= limit) currentFile.fileSize else 0) + 
      currentFile.children.foldLeft(0)((acc, child) => {
        acc + (if (child.isDirectory) getAllFilesUnderLimit(child, limit) else 0)
    })
  }
  
  private def findSmallestFileToDelete(currentFile: File, freeSpace: Int): Int = {
    if (currentFile.children == null) {
      return Int.MaxValue
    }
    val smallestChildToDelete: Int = currentFile.children.map(child => {
      findSmallestFileToDelete(child, freeSpace)
    }).min
    
    if (currentFile.fileSize + freeSpace >= DESIRED_FREE_SPACE) {
      return Math.min(currentFile.fileSize, smallestChildToDelete)
    }
    smallestChildToDelete
  }
  
  def solvePartOne(input: Array[String]): Int = {
    val fileSystem = createFileStructure(input)
    
    getAllFilesUnderLimit(fileSystem, 100000)
  }

  def solvePartTwo(input: Array[String]): Int = {
    val fileSystem = createFileStructure(input)
    
    val freeSpace = AVAILABLE_SPACE - fileSystem.getFileSize
    findSmallestFileToDelete(fileSystem, freeSpace)
  }
}
