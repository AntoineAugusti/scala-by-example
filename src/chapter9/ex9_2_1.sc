package chapter9

object ex9_2_1 {

  def lengthList(lst: List[Int]): Int = {
    def iter(lst: List[Int], acc: Int) : Int = {
      if (lst.isEmpty) acc
      else iter(lst.tail, acc + 1)
    }
    iter(lst, 0)
  }

  // Length non empty list
  assert(lengthList(List(1, 2)) == 2)
  // Length empty list
  assert(lengthList(List()) == 0)
}