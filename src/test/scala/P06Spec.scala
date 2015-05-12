package org.p99.scala

import org.scalatest._

class P06Spec extends UnitSpec {

  "isPalindrome" should "be true for palindrome" in {
    P06.isPalindrome(List('a, 'b', 'a)) should be (true)
  }

  "isPalindrome" should "be false for non palindrome" in {
    P06.isPalindrome(List('a, 'b)) should be (true)
  }

}

