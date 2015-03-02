package chapter7

object ex_7_2_2 {
  val treeOneTwo = EmptyTree insert 1 insert 2

  // Test contains
  assert(treeOneTwo contains 1)
  assert(treeOneTwo contains 2)
  assert(!(treeOneTwo contains 0))

  // Test insert
  assert(EmptyTree insert 1 contains 1)
  assert(treeOneTwo insert 3 contains 3)
}

abstract class IntTree {
  def contains(v: Int): Boolean = this match {
    case EmptyTree => false
    case Node(elem, left, right) => {
      if (v == elem) true
      else if (v < elem) left contains v
      else right contains v
    }
  }

  def insert(v: Int): IntTree = this match {
    case EmptyTree => new Node(v, EmptyTree, EmptyTree)
    case Node(elem, left, right) => {
      if (elem == v) this
      else if (v < elem) new Node(elem, left insert v, right)
      else new Node(elem, left, right insert v)
    }
  }
}

case object EmptyTree extends IntTree
case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree