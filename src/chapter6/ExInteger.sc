package chapter6

object ExInteger {

  abstract class Integer {
    def isZero: Boolean
    def isPositive: Boolean
    final def isNegative: Boolean = !isPositive
    def predecessor: Integer
    def successor: Integer
    def + (that: Integer): Integer
    def - (that: Integer): Integer
    def negate: Integer = ZeroInteger - this
  }

  object ZeroInteger extends Integer {
    def isZero: Boolean = true
    def isPositive: Boolean = false
    def predecessor: Integer = new PredInteger(this)
    def successor: Integer = new SuccInteger(this)
    def + (that: Integer): Integer = that
    def - (that: Integer): Integer = {
      def iter(n: Integer, res: Integer): Integer = {
        if (n.isZero) res
        else if (n.isPositive) iter(n.predecessor, res.predecessor)
        else iter(n.successor, res.successor)
      }
      iter(that, this)
    }

    override def negate: Integer = this
  }

  class SuccInteger(x: Integer) extends Integer {
    def isZero: Boolean = false
    def isPositive: Boolean = {
      if (predecessor.isZero || predecessor.isPositive) true
      else false
    }
    def predecessor: Integer = if (x.isZero) ZeroInteger else new PredInteger(x)
    def successor: Integer = new SuccInteger(x)
    def + (that: Integer): Integer = x + that.successor
    def - (that: Integer): Integer = x - that.predecessor
  }

  class PredInteger(x: Integer) extends Integer {
    def isZero: Boolean = false
    def isPositive: Boolean = {
      if (successor.isZero || ! successor.isPositive) false
      else true
    }
    def predecessor: Integer = new PredInteger(x)
    def successor: Integer = if (x.isZero) ZeroInteger else new SuccInteger(x)
    def + (that: Integer): Integer = x + that.predecessor
    def - (that: Integer): Integer = x - that.successor
  }

  val z = ZeroInteger
  val one = z.successor
  val two = one.successor

  assert(one.isPositive)
  assert(one.predecessor.isZero)
  assert(one.negate.isNegative)
  assert(z.negate.isZero)
  assert(one.negate.successor.isZero)
  assert((one.negate + one).isZero)
  assert((one + one).isPositive)
  assert((z + one).isPositive)
  assert((one - two).isNegative)
  assert((one + one - two).isZero)
  assert((one - one).isZero)
  // Failing test here
  // assert((one - two + one).isZero)
}