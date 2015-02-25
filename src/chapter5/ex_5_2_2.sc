package chapter5

object ex_5_2_2 {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(a: Int, acc: Int): Int = {
      if (a > b) acc
      else iter(a + 1, f(a) * acc)
    }
    iter(a, 1)
  }
  def id(x: Int) : Int = x
  product(id)(3, 5)                         //> res0: Int = 60
}