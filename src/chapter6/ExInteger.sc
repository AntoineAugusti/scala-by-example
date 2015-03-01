package chapter6

object ExInteger {

  abstract class Integer {
    def isZero: Boolean
    def isPositive: Boolean
    def predecessor: Integer
    def successor: Integer
    def + (that: Integer): Integer
    def - (that: Integer): Integer
    def negate: Integer = ZeroInteger - this
    final def isNegative: Boolean = !isPositive
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
    def isPositive: Boolean = true
    def predecessor: Integer = x
    def successor: Integer = new SuccInteger(this)
    def + (that: Integer): Integer = x + that.successor
    def - (that: Integer): Integer = x - that.predecessor
  }

  class PredInteger(x: Integer) extends Integer {
    def isZero: Boolean = false
    def isPositive: Boolean = false
    def predecessor: Integer = new PredInteger(this)
    def successor: Integer = x
    def + (that: Integer): Integer = x + that.predecessor
    def - (that: Integer): Integer = x - that.successor
  }

  // Create some useful values to test
  val z = ZeroInteger
  val one = z.successor
  val two = one.successor
  val three = two.successor

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
  assert((one - two + one).isZero)
  assert((one - three + two).isZero)
}