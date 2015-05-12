package org.p99.scala

import org.scalatest._

class P19Spec extends UnitSpec {
  "rotate" should "rotate a list N to the left" in {
    P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  }

  it should "rotate a list to the right given negative n" in {
    P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }
}

