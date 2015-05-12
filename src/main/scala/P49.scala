package org.p99.scala

import scala.collection.mutable

object P49 extends App {
  def gray(n: Int): List[String] = {
    val memo = mutable.Map[Int, List[String]]()

    def internal(n: Int) = n match {
      case 0 => List("")
      case _ => gray(n - 1).flatMap(str =>
        for {
          x <- List('0', '1')
        } yield str + x)
    }

    memo.get(n) match {
      case Some(x) => x
      case None =>
        val result = internal(n)
        memo(n) = result
        result
    }
  }
  println(gray(10))
}
