package chapter10

object ex10_3_1 {
  def flatten[A](xss: List[List[A]]): List[A] =
    (xss :\ (Nil: List[A])) ((xs, ys) => xs ::: ys)

  def myFlatten[A](xss: List[List[A]]): List[A] =
    for (xs <- xss; x <- xs) yield x

  val list = List(List(1, 2), List(3))
  val target = List.range(1, 4)

  assert(flatten(list) == target)
  assert(myFlatten(list) == target)
}