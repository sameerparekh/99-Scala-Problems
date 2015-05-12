package org.p99.scala

object P38 extends App {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
  time { P37.phi(1009000) }
  time { P34.totient(1009000) }
}
