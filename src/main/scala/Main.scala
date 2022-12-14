import util.GenericSolution

object Main {
  private val filePathFormat = "day%d/input.txt"


  def main(args: Array[String]): Unit = {
    runDayOne()
    runDayTwo()
    runDayThree()
    runDayFour()
    runDayFive()
    runDaySix()
    runDaySeven()
//    runDayEight()
    runDayTen()
    runDayEleven()
  }

  private def runDay(solver: GenericSolution, dayNumber: Int): Unit = {
    println(s"======= Running Day $dayNumber =======")
    val inputFilePath = filePathFormat.format(dayNumber)
    val beforeTime = System.nanoTime()
    solver.solve(solver.readFile(inputFilePath))
    val afterTime = System.nanoTime()
    println(s"Took ${(afterTime - beforeTime) / 1e9d}s to run")
    println(s"======= Finished Running Day $dayNumber =======")
  }

  def runDayOne(): Unit = {
    val solver = new day1.Solution()
    runDay(solver, 1)
  }

  def runDayTwo(): Unit = {
    val solver = new day2.Solution()
    runDay(solver, 2)
  }

  def runDayThree(): Unit = {
    val solver = new day3.Solution()
    runDay(solver, 3)
  }

  def runDayFour(): Unit = {
    val solver = new day4.Solution()
    runDay(solver, 4)
  }

  def runDayFive(): Unit = {
    val solver = new day5.Solution()
    runDay(solver, 5)
  }

  def runDaySix(): Unit = {
    val solver = new day6.Solution()
    runDay(solver, 6)
  }

  def runDaySeven(): Unit = {
    val solver = new day7.Solution()
    runDay(solver, 7)
  }

  def runDayEight(): Unit = {
    val solver = new day8.Solution()
    runDay(solver, 8)
  }

  def runDayTen(): Unit = {
    val solver = new day10.Solution()
    runDay(solver, 10)
  }

  def runDayEleven(): Unit = {
    val solver = new day11.Solution()
    runDay(solver, 11)
  }
}
