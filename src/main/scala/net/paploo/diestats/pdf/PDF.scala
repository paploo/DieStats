package net.paploo.diestats.pdf

import scala.collection.immutable.SortedMap

object PDF {
  def empty[T](implicit ord: Ordering[T]) = new PDF[T]( SortedMap.empty(ord) )

  def apply[T](elems: (T, Double)*)(implicit ord: Ordering[T]): PDF[T] = new PDF[T](SortedMap[T, Double](elems: _*)(ord))

  def apply[T](map: Map[T, Double])(implicit ord: Ordering[T]): PDF[T] = new PDF[T](SortedMap[T, Double](map.toSeq: _*)(ord))

  def fromMap[T, U <: Double](map: scala.collection.Map[T, U])(implicit ord: Ordering[T]): PDF[T] = new PDF[T](SortedMap(map.toSeq: _*)(ord))
}

class PDF[T] private(map: SortedMap[T, Double]) extends Iterable[T] {

  def minKey(implicit ord: Ordering[T]): T = map.keys.min(ord)

  def maxKey(implicit ord: Ordering[T]): T = map.keys.max(ord)

  override def iterator: Iterator[T] = Range(minKey, maxKey)

}
