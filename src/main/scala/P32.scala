package org.p99.scala

object P32 extends App {
  def gcd(a: Int, b: Int): Int = {
    if (a < b)
      gcd(b, a)
    else {
      val q = a / b
      val r = a % b
      if (r == 0) b else gcd(b, r)
    }
  }
}
