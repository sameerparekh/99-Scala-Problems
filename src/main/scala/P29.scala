package org.p99.scala

object P29 {
  def lsort[T](l: List[List[T]]): List[List[T]] = {
    l.sortWith((a, b) => a.length < b.length)
  }

}
