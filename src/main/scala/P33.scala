package org.p99.scala

object P33 {
  def isCoprimeTo(a: Int, b: Int): Boolean = P32.gcd(a, b) == 1
}
