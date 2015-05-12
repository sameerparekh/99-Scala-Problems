package org.p99.scala

object P47 extends App {
  implicit class TruthOperators(a: Boolean) {
    def and(b: Boolean) = P46.and(a, b)
    def or(b: Boolean) = P46.or(a, b)
    def nand(b: Boolean) = P46.nand(a, b)
    def nor(b: Boolean) = P46.nor(a, b)
    def xor(b: Boolean) = P46.xor(a, b)
    def impl(b: Boolean) = P46.impl(a, b)
    def equ(b: Boolean) = P46.equ(a, b)
  }
  def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  P46.table2((a: Boolean, b: Boolean) => not(a xor b))
}
