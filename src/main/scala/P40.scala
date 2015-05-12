package org.p99.scala

object P40 extends App {
  def goldbach(g: Int): (Int, Int) = {
    require(g % 2 == 0 && g > 2, "must be even and greater than 2")
    val primesInRange = P39.listPrimesinRange(1 to g)
    val left = primesInRange.filter(primesInRange contains g - _).head
    (left, g - left)
  }
}
