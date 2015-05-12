package org.p99.scala

object P36 extends App {
  def primeFactorMultiplicity(x: Int) = P10.encode(P35.primeFactors(x)).map( { case (x, y) => (y, x) }).toMap
}
