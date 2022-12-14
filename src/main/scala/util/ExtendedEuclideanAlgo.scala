package util

import scala.annotation.tailrec

/*
 * Reference: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
 */
object ExtendedEuclideanAlgo {

  // Represents one iteration in the algorithm
  case class Iteration(
                        r: Int, rPrime: Int,
                        x: Int, xPrime: Int,
                        y: Int, yPrime: Int
                      )

  // Recursively execute the algorithm and finally return (x, y)
  @tailrec
  def EEA(i: Iteration): (Int, Int) =
    if (i.rPrime != 0) {
      val d = Math.floor(i.r / i.rPrime).toInt

      val next =
        Iteration(
          i.rPrime, i.r - d * i.rPrime,
          i.xPrime, i.x - d * i.xPrime,
          i.yPrime, i.y - d * i.yPrime
        )

      EEA(next)
    } else
      (i.x, i.y) //> EEA: (i: ExtendedEuclideanAlgorithm.Iteration)(Int, Int)

  def EEA(a: Int, b: Int): (Int, Int) = EEA(Iteration(a, b, 1, 0, 0, 1))
  
  def getMultiplicativeInverse(modulus: Int, divisor: Int): Int = {
    val result = EEA(modulus, divisor)
    result._2
  }
}