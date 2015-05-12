package org.p99.scala

object P35 extends App {

  def primesFun(nums: Stream[Int]): Stream[Int] = nums match {
    case x #:: xs => Stream.cons(x, primesFun(xs.filter(_ % x != 0)))
    case _ => Stream.empty
  }
  lazy val primes = primesFun(Stream.from(2))

  def primeFactors(x: Int): List[Int] = x match {
    case 1 => List.empty
    case _ =>
      val factor: Int = primes.filter(x % _ == 0).head
      factor :: primeFactors(x / factor)
  }
  println(primeFactors(315))
}
