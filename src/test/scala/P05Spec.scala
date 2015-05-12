package org.p99.scala

import org.scalatest._

class P05Spec extends UnitSpec {

  "reverse" should "reverse the list" in {
    assert(P05.reverse(List('a, 'b, 'c)) === List('c, 'b, 'a))
  }

  "reverse" should "work on empty list" in {
    assert(P05.reverse(List()) === List())
  }

}

