package org.p99.scala

object P39 extends App {


  def listPrimesinRange(range: IndexedSeq[Int]) = P35.primesFun((2 to range.last).toStream).filter(range contains _).toList
  println(listPrimesinRange(115 to 405))
}
