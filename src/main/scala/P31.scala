package org.p99.scala
import scala.math.sqrt

object P31 extends App {
  def isPrime(x: Int): Boolean = x match {
    case 1 => true
    case _ =>
      (2 to sqrt(x).toInt).forall(x % _ != 0)
  }
}
