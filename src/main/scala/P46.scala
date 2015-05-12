package org.p99.scala

object P46 extends App {
  def and(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean) = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(a: Boolean, b: Boolean) = and(a, b) match {
    case true => false
    case false => true
  }

  def nor(a: Boolean, b: Boolean) = or(a, b) match {
    case true => false
    case false => true
  }

  def xor(a: Boolean, b: Boolean) = (a, b) match {
    case (true, false) => true
    case (false, true) => true
    case _ => false
  }

  def impl(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case (true, false) => false
    case (false, _) => true
  }

  def equ(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case (false, false) => true
    case _ => false
  }

  def table2(expr: (Boolean, Boolean) => Boolean) = {
    println("A\tB\t\result")
    val results = for {
      a <- List(true, false)
      b <- List(true, false)
    } yield (a, b, expr(a, b))
    results.foreach { case (a, b, result) => println(s"$a\t$b\t$result") }
  }

  table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
}
