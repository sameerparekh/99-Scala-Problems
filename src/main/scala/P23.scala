package org.p99.scala
import scala.util.Random

object P23 {
  val r = new Random()

  def randomSelect[T](n: Int, ls: List[T]): List[T] = n match {
    case 0 => List()
    case x =>
      val ix = r.nextInt(ls.length)
      val (newList, sel) = P20.removeAt(ix, ls)
      sel :: randomSelect(x - 1, newList)
  }

}
