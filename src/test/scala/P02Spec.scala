package org.p99.scala

import org.scalatest._

class P02Spec extends UnitSpec {

  "penultimate" should "return second last item" in {
    assert(P02.penultimate(List('a, 'b, 'c, 'd)) === 'c)
  }

  it should "throw on an empty list" in {
    intercept[NoSuchElementException] {
      P02.penultimate(List())
    }
  }

  it should "throw on a list with 1 element" in {
    intercept[NoSuchElementException] {
      P02.penultimate(List('a))
    }
  }

}

