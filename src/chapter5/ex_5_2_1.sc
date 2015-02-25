package chapter5

object ex_5_2_1 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(a: Int, acc: Int): Int = {
      if (a > b) acc
      else iter(a + 1, f(a) + acc)
    }
    iter(a, 0)
  }
  def id(x: Int) : Int = x
  def square(x: Int) : Int = x*x
  sum(id)(1, 5)                             //> res0: Int = 15
}