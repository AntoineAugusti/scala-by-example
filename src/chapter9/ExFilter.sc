package chapter9

object ExFilter {
  def forAllFilter(lst: List[Int], p: Int => Boolean): Boolean =
    lst.isEmpty || lst.filter(p).length == lst.length

  def existsFilter(lst: List[Int], p: Int => Boolean): Boolean =
    lst.filter(p).length >= 1

  // Test forall
  assert(forAllFilter(List(1, 2), (x => x >= 1)))
  assert(forAllFilter(List(), (x => x > 0)))
  assert(!(forAllFilter(List(1, 2), (x => x >= 2))))

  // Test exists
  assert(existsFilter(List(1, 2), (x => x == 2)))
  assert(!existsFilter(List(1), (x => x > 1)))
}