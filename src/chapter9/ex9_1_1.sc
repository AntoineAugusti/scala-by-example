package chapter9

object ex9_1_1 {

  def isort(xs: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] =
      if (xs.isEmpty) List(x)
      else if (x < xs.head) x :: xs
      else xs.head :: insert(x, xs.tail)

    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))
  }

  // Sort an empty list
  val empty = List()
  assert(isort(empty).isEmpty)

  // Sort a random list of integers
  val numbers = List(13, 1337, 32, 42)
  val sorted = isort(numbers)
  assert(sorted.head == 13)
  assert(sorted.tail.head == 32)
  assert(sorted.tail.tail.head == 42)
  assert(sorted.tail.tail.tail.head == 1337)
}