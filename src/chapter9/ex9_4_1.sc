package chapter9

object ex9_4_1 {
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

  def squareListMap(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  val oneTwoList = List.range(1, 3)

  // Test with pattern matching
  assert(squareList(List()).isEmpty)
  assert(squareList(oneTwoList).head == 1)
  assert(squareList(oneTwoList).tail.head == 4)

  // Test the map version
  assert(squareListMap(List()).isEmpty)
  assert(squareListMap(oneTwoList).head == 1)
  assert(squareListMap(oneTwoList).tail.head == 4)
}