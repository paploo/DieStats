package net.paploo.diestats.util

object OrderingSupport {

  /**
    * The null ordering is useful for when no ordering needs to take place, such as during
    * convolution over the domains.
    */
  def nullOrdering[A]: Ordering[A] = new Ordering[A] {
    override def compare(x: A, y: A): Int = 0
  }

  /**
    * A natural ordering of Sets, based on the ordering of their elements.
    *
    * The sorting rules:
    * - Sets that are equal are, obviously, equal,
    * - Sets that are shorter sort first, and
    * - Other sets are compared based on the lowest non-equal element sort order.
    *
    * @param ord The ordering of the set elements
    */
  implicit def setOrdering[A](implicit ord: Ordering[A]): Ordering[Set[A]] = new Ordering[Set[A]] {
    override def compare(x: Set[A], y: Set[A]): Int =
      if (x == y) 0
      else if (x.size != y.size) y.size - x.size
      else {
        val sortedX = x.toSeq.sorted
        val sortedY = y.toSeq.sorted
        (0 until x.size).find { i =>
          ord.compare(sortedX(i), sortedY(i)) != 0
        }.map { i =>
          ord.compare(sortedX(i), sortedY(i))
        }.get
      }
  }

  trait Implicits {

    implicit def setOrdering[A](implicit ord: Ordering[A]): Ordering[Set[A]] = OrderingSupport.setOrdering

  }

  object Implicits extends Implicits

}
