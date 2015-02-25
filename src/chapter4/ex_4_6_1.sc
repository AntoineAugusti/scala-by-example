package chapter4

object ex_4_6_1 {
  def factorial(n: Int): Int = {
    assert(n >= 0)

    def factorialIter(curr: Int, acc: Int) : Int = {
      if (curr > n) acc
      else factorialIter(curr + 1, acc * curr)
    }
    factorialIter(1, 1)
  }                                               //> factorial: (n: Int)Int

  factorial(5)                                    //> res0: Int = 120
}