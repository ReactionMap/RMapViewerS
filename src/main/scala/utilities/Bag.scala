package utilities

import scala.collection.immutable.Map

class Bag[T](xs: Iterable[T]) {
  val contents: Map[T, Int] = xs.foldLeft(Map(): Map[T, Int])((m: Map[T, Int], x: T) =>
    m + (x -> (m.get(x) match {
      case None => 1
      case Some(n) => n + 1
    })))

  def isSubbagOf(b: Bag[T]): Boolean =
    !contents.exists((assoc) =>
      b.contents.get(assoc._1) match {
        case None => true
        case Some(n2) => assoc._2 > n2
      })
  def occurrencesOf(x: T): Int = contents.getOrElse(x, 0)
}
