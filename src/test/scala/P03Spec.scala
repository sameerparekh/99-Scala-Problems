package org.p99.scala

import org.scalatest._

class P03Spec extends UnitSpec {
  "nth" should "return nth item in list" in {
    assert(P03.nth(3, List('a, 'b, 'c, 'd)) === 'd)
  }

  it should "throw when n > size of list" in {
    intercept[NoSuchElementException] {
      P03.nth(5,  List('a, 'b, 'c))
    }
  }

  it should "throw on negative n" in {
    intercept[NoSuchElementException] {
      P03.nth(-1, List('a, 'b, 'c))
    }
  }
}

