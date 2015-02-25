package chapter4

import math.abs
import math.ulp

object exercice_4_4_1 {
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(x - square(guess)) <= ulp(x)

    def square(x: Double) = x * x

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                 //> sqrt: (x: Double)Double

  val x = sqrt(2)                   //> x  : Double = 1.414213562373095
}