package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.domain.DomainOps

import scala.language.higherKinds

/**
  * The base trait for distributions over the domain A.
  *
  * @tparam A The domain type.
  * @tparam B The value for an input from the domain; this is typically a numeric type.
  */
trait Distribution[A, +B] extends PartialFunction[A, B] {

  override def apply(a: A): B = get(a).getOrElse(throw new NoSuchElementException(s"Value undefined for domain value $a"))

  override def isDefinedAt(a: A): Boolean = get(a).isDefined

  def get(a: A): Option[B]

  /**
    * Gets the domain list, sorted according to DomainOps.
    */
  def domain(implicit dops: DomainOps[A]): Seq[A] = this.toMap.keys.toSeq.sorted(dops.ordering)

  /**
    * Gets the pairs, sorted according to DomainOps.
    */
  def pairs(implicit dops: DomainOps[A]): Seq[(A, B)] = this.toMap.toSeq.sortBy(_._1)(dops.ordering)

  /**
    * Returns the distribution as a simple Map[A, B]
    */
  def toMap: Map[A, B]

  /**
    * Returns the distribution as a Sequence of pairs
    */
  def toSeq: Seq[(A, B)] = toMap.toSeq

}

/**
  * Distributions with a concrete range type should have companions that implement these methods.
  * @tparam B The concrete range type.
  * @tparam Repr The representation higher type.
  */
trait ConcreteDistributionCompanion[B, Repr[_]] {

  def empty[A]: Repr[A]

  def apply[A](pairs: (A, B)*): Repr[A] = buildFrom(pairs)

  def buildFrom[A](pairs: TraversableOnce[(A, B)]): Repr[A]

}