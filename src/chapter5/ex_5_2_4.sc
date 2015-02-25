package chapter5

object ex_5_2_4 {
  def compute(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), compute(f, combine, zero)(a+1, b))
  }
  def product(f: Int => Int)(a: Int, b: Int): Int = compute(f, (x, y) => x*y, 1)(a, b)
  def sum(f: Int => Int)(a: Int, b: Int): Int = compute(f, (x, y) => x+y, 0)(a, b)

  def id(x: Int): Int = x
  product(id)(1, 5)                         //> res0: Int = 120
  sum(id)(1, 5)                             //> res1: Int = 15
}