package day11

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.write
import util.{ExtendedEuclideanAlgo, GenericSolution}

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer

case class Item(moduli: List[Int],
                inverses: List[Int],
                remainders: ListBuffer[Int]) {
  def doOperation(operation: String): Unit = {
    val newRemainders = remainders.zipWithIndex.map(worryInfo => {
      val largeWorryLevel: Int = if (operation.contains("+")) {
        operation.replaceAll("old", worryInfo._1.toString).split('+').foldLeft(0)((acc, value) => acc + value.toInt)
      } else {
        operation.replaceAll("old", worryInfo._1.toString).split('*').foldLeft(1)((acc, value) => acc * value.toInt)
      }
      (largeWorryLevel * inverses(worryInfo._2)) % moduli(worryInfo._2)
    })
//    println(s"New Remainders: $newRemainders")
    Range(0, remainders.length).foreach(index => remainders(index) = newRemainders(index))
  }
}

object Item {
  def apply(itemValue: Int, moduli: List[Int], inverses: List[Int]): Item = {
    val remainders = moduli.map(itemValue % _)
    new Item(moduli, inverses, ListBuffer.from(remainders))
  }
}

case class Monkey(items: ListBuffer[Item],
                  operation: String,
                  testDivisibleBy: Int,
                  trueTestMonkey: Int,
                  falseTestMonkey: Int,
                  id: Int,
                  var inspections: Long) {
  implicit val formats: DefaultFormats.type = DefaultFormats

  override def toString: String = write(this)

  def inspectItem(item: Item): (Item, Int) = {
    item.doOperation(operation)
    val newWorryLevel = item.remainders(id)
    (item, if (newWorryLevel % testDivisibleBy == 0) trueTestMonkey else falseTestMonkey)
  }
  
  // _1 is the new worry level
  // _2 is the new monkey that the item will be transferred to
  def inspectItems: Array[(Item, Int)] = {
//    println(items)
    this.inspections += items.length
    val result = items.foldLeft(Array[(Item, Int)]())((inspectionInfo, worryLevel) => 
      inspectionInfo :+ inspectItem(worryLevel))
    items.clear()
    result
  }
}

class Solution extends GenericSolution {
  val STARTING_ITEM_PATTERN = "Starting items: "
  val OPERATION_PATTERN = "Operation: new = "
  val TEST_PATTERN = "Test: divisible by "
  val TRUE_MONKEY_PATTERN = "If true: throw to monkey "
  val FALSE_MONKEY_PATTERN = "If false: throw to monkey "
  
  def processMonkey(monkeyInfo: Array[String], monkeyId: Int, modulis: List[Int], inverses: List[Int]): Monkey = {
    val itemValues = ListBuffer().addAll(monkeyInfo(1).trim.substring(STARTING_ITEM_PATTERN.length).split(",").map(_.trim.toInt))
    val items = itemValues.map(itemValue => Item.apply(itemValue, modulis, inverses))
    val operation = monkeyInfo(2).trim.substring(OPERATION_PATTERN.length).replaceAll(" ", "")
    val testDivisibleBy = monkeyInfo(3).trim.substring(TEST_PATTERN.length).toInt
    val trueTestMonkey = monkeyInfo(4).trim.substring(TRUE_MONKEY_PATTERN.length).toInt
    val falseTestMonkey = monkeyInfo(5).trim.substring(FALSE_MONKEY_PATTERN.length).toInt
    Monkey(items, operation, testDivisibleBy, trueTestMonkey, falseTestMonkey, monkeyId, 0)
  }
  
  def doRoundsOfInspection(rounds: Int, monkeys: Array[Monkey], divideByThree: Boolean = true): Unit = {
    Range(0, rounds).foreach(_ => {
      monkeys.foreach(monkey => {
        val newMonkeys = monkey.inspectItems
        newMonkeys.foreach(newMonkey => {
          monkeys(newMonkey._2).items.addOne(newMonkey._1)
        })
      })
    })
  }
  
  def createInverses(moduli: List[Int], divisor: Int): List[Int] = {
    moduli.map(modulus => {
      val inverse = ExtendedEuclideanAlgo.getMultiplicativeInverse(modulus, divisor)
      if (inverse < 0) inverse + modulus else inverse
    })
  }
  
  def getDivisibleTestValues(input: Array[String]): List[Int] = {
    input.filter(_.contains(TEST_PATTERN)).map(_.trim.substring(TEST_PATTERN.length).toInt).toList
  }
  
  def createMonkeys(input: Array[String], divisor: Int): Array[Monkey] = {
    val moduli = getDivisibleTestValues(input)
    val inverses = createInverses(moduli, divisor)
    input.grouped(7).zipWithIndex.foldLeft(Array[Monkey]())((array, monkey) =>
      array :+ processMonkey(monkey._1, monkey._2, moduli, inverses))
  }

  def solvePartOne(input: Array[String]): Long = {
    val roundsOfInspection = 20
    val monkeys = createMonkeys(input, 3)
    doRoundsOfInspection(roundsOfInspection, monkeys)
    
    val sortedInspections = monkeys.foldLeft(TreeSet[Long]()(Ordering.Long.reverse))((treeSet, monkey) => {
      treeSet + monkey.inspections
    })
    println(sortedInspections)
    sortedInspections.head * sortedInspections.tail.head
  }

  def solvePartTwo(input: Array[String]): Long = {
    val roundsOfInspection = 10000
    val monkeys = createMonkeys(input, 1)
    doRoundsOfInspection(roundsOfInspection, monkeys)

    val sortedInspections = monkeys.foldLeft(TreeSet[Long]()(Ordering.Long.reverse))((treeSet, monkey) => {
      treeSet + monkey.inspections
    })
//    println(sortedInspections)
    sortedInspections.head * sortedInspections.tail.head
  }
}