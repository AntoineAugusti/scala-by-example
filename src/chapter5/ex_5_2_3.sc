package chapter5

object ex_5_2_3 {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(a: Int, acc: Int): Int = {
      if (a > b) acc
      else iter(a + 1, f(a) * acc)
    }
    iter(a, 1)
  }
  def factorial(nb: Int): Int = product(x => x)(1, nb)

  factorial(5)                              //> res0: Int = 120
}