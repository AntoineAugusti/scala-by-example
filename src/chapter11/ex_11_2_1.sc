package chapter11

object ex_11_2_1 {

  def repeatLoop(command: => Unit)(condition: => Boolean) {
    if (condition) {
      command; repeatLoop{command}(condition)
    } else ()
  }

  def until(condition: => Boolean): Boolean = !condition

  var x = 0

  // Test the repeat loop
  repeatLoop{x += 1}(x < 100)
  assert(x == 100)

  // Test the repeat loop with until
  repeatLoop{x -= 1}(until (x == 0))
  assert(x == 0)
}