package org.p99.scala

import org.scalatest._

class P01Spec extends UnitSpec {

  "last" should "return last item of list" in {
    assert(P01.last(List('a, 'b, 'c, 'd)) === 'd)
  }

  it should "throw on an empty list" in {
    intercept[NoSuchElementException] {
      val l = P01.last(List())
    }
  }
}

