package org.p99.scala

object P30 extends App {
  def lsortFreq[T](l: List[List[T]]): List[List[T]] = {
    val grouped = l.groupBy(_.length).values.toList
    P29.lsort(grouped).flatten
  }
  println(lsortFreq(List(List('a, 'b), List('b, 'c, 'd), List('c, 'd))))

}
