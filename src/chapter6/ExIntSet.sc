package chapter6

object ExIntSet {
  trait IntSet {
    def incl(x: Int): IntSet // Add x to the set
    def contains(x: Int): Boolean // Is x in the set?
    def union(other: IntSet): IntSet // Union of the set and another one
    def intersection(other: IntSet): IntSet // Intersection of the set and another one
  }

  class EmptySet extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
    def union(other: IntSet): IntSet = other
    def intersection(other: IntSet): IntSet = this
  }

  class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmptySet(elem, left incl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet = ((left union right) union other) incl elem
    def intersection(other: IntSet): IntSet = {
      val newSet = (right intersection other) union (left intersection other)
      // The elem was already here, return the set
      if (newSet contains elem) newSet
      // Otherwise add the current elem
      else newSet incl elem
    }
  }

  // Arrange
  val one = (new EmptySet) incl 1
  val two = (new EmptySet) incl 2

  // Test union
  val oneTwo = one union two
  assert(oneTwo contains 1)
  assert(oneTwo contains 2)

  // Test intersection
  val oneIntersection = one intersection oneTwo
  assert(oneIntersection contains 1)
  assert(!(oneIntersection contains 2))
}