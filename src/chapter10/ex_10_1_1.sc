package chapter10

object ex10_1_1 {
  def queens(n: Int): List[List[Int]] = {
      def placeQueens(k: Int): List[List[Int]] =
        if (k == 0) List(List())
        else for {queens <- placeQueens(k - 1)
          column <- List.range(1, n + 1)
          if isSafe(column, queens, 1)} yield column :: queens
    placeQueens(n)
  }

  // Tests whether a queen in the given column col is safe
  // with respect to the queens already placed.
  // Delta is the difference between the row of the queen to be placed
  // and the row of the first queen in the list.
  def isSafe(column: Int, queens: List[Int], delta: Int): Boolean = queens match {
    case Nil => true
    case col :: rest =>
      col != column && math.abs(col - column) != delta && isSafe(column, rest, delta + 1)
  }

  queens(4)
  queens(8).length
}