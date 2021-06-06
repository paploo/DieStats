package net.paploo.diestats.statistics.distribution

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * The base trait for discrete distributions over the domain A.
  *
  * This library, being made for dice statistics, focuses entirely on
  * discrete distributions, so we dispense with the "Discrete" prefix.
  *
  * Typically, the domain is considered to satisfy `Monoid` and `Ordering`,
  * while the values are considered to satisfy `Numeric`, however the
  * infrastructure defers to typeclasses for this behavior only when necessary.
  *
  * @tparam A The domain type.
  * @tparam B The value for an input from the domain; this is typically a numeric type.
  */
trait Distribution[A, +B] extends PartialFunction[A, B] {

  override def apply(a: A): B = get(a).getOrElse(throw new NoSuchElementException(s"Value undefined for domain value $a"))

  override def isDefinedAt(a: A): Boolean = get(a).isDefined

  /**
    * Returns Some(f) where f is the frequency for a valid domain value, otherwise None.
    *
    * Distributions should endeavor to specify a zero frequency in order
    * to return Some(a) for all legal domain values a, otherwise None
    * will be returned.
    */
  def get(a: A): Option[B]

  /**
    * Gets the domain list, sorted according to DomainOps.
    */
  def domain(implicit ord: Ordering[A]): Seq[A] = this.toMap.keys.toSeq.sorted(ord)

  /**
    * Gets the pairs, sorted according to DomainOps.
    */
  def pairs(implicit ord: Ordering[A]): Seq[(A, B)] = this.toMap.toSeq.sortBy(_._1)(ord)

  /**
    * The count of the pairs, which is identical to the domain length.
    * @return
    */
  def size: Int

  /**
    * Returns the distribution as a simple Map[A, B]
    */
  def toMap: Map[A, B]

  /**
    * Returns the distribution as a Sequence of pairs
    */
  def toSeq: Seq[(A, B)] = toMap.toSeq

  /**
    * The same pairs, regardles sof order, should
    * produce the same hash code.
    * @return
    */
  override def hashCode(): Int = toMap.hashCode

  /**
    * Equality for distributions is defined as when their pairs,
    * in any order, are equivalent; comparison against other
    * iterables with equivalent pairs results in inequality.
    * @param obj
    * @return
    */
  override def equals(obj: Any): Boolean = obj match {
    case dist: Distribution[_, _] => this.toMap == dist.toMap
    case _ => false
  }
}

object Distribution {

  implicit def distributionToIterable[A, B](dist: Distribution[A, B]): Iterable[(A, B)] =
    dist.toSeq

  /**
    * Reduces the given pairs by summing the numeric when the pair is equal.
    *
    * Note that the ordering of the resulting pairs is not preserved and is unpredictable.
    * @param pairs
    * @param num
    * @tparam A
    * @tparam N
    * @return
    */
  def reducePairs[A, N](pairs: Iterable[(A, N)])(implicit num: Numeric[N]): Iterable[(A, N)] = {
    val buffer = mutable.Map.empty[A, N]

    pairs.foreach { case (a,n) =>
      val oldN = buffer.getOrElseUpdate(a, num.zero)
      val newN = num.plus(oldN, n)
      buffer += a -> newN
    }

    buffer.toMap
  }

}

/**
  * Distributions with a concrete range type should have companions that implement these methods.
  * @tparam B The concrete range type.
  * @tparam Repr The representation higher type.
  */
trait DistributionCompanion[B, Repr[_]] {

  def empty[A]: Repr[A]

  def apply[A](pairs: (A, B)*): Repr[A] = buildFrom(pairs)

  def buildFrom[A](pairs: Iterable[(A, B)]): Repr[A]

}