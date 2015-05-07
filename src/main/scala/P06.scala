package org.p99.scala

object P06 {
  def isPalindrome[T](l: List[T]): Boolean = P05.reverse(l) == l
}
