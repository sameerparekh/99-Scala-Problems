package org.p99.scala

object P41 extends App {
  def goldbachList(range: IndexedSeq[Int]) = range.filter(_ % 2 == 0).map(P40.goldbach)
  def printGoldbach(gb: (Int, Int)) = gb match { case (x, y) => val sum = x + y; println(s"$sum = $x + $y") }
  def printGoldbachList(range: IndexedSeq[Int]) = goldbachList(range).foreach(printGoldbach)
  def printGoldbachListLimited(range: IndexedSeq[Int], limit: Int) = goldbachList(range).filter {
    case (x, y) if x > limit && y > limit => true
    case _ => false
  }.foreach(printGoldbach)

  //printGoldbachList(9 to 20)
  printGoldbachListLimited(3 to 3000, 50)
}
