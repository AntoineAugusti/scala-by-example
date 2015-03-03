package chapter9

object ex9_4_3 {
  def mapFun[A, B](xs: List[A], f: A => B) : List[B] =
    xs.foldRight(List[B]())({(x, xs) => f(x) :: xs})

  def lengthFun[A](xs: List[A]): Int =
    xs.foldLeft(0)({(count, actual) => count + 1})

  val oneToThree: List[Int] = List(1, 2, 3)
  def square(x: Int): Int = x * x
  assert(mapFun(oneToThree, square) == List(1, 4, 9))
  assert(lengthFun(oneToThree) == 3)
}