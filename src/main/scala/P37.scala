package org.p99.scala

object P37 {
  def phi(m: Int) = {
    val factors = P36.primeFactorMultiplicity(m).toList
    def pow(a: Int, b: Int): Int = {
      require(b >= 0, "exponent must be non-negative integer")
      b match {
        case 0 => 1
        case _ => a * pow(a, b - 1)
      }
    }

    val phiElems = factors.map {
      {
        case (p: Int, m: Int) => (p - 1) * pow(p, m - 1)
      }
    }
    phiElems.product
  }

}
