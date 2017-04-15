package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.domain.DomainOps

/**
  * The base trait for distributions over the domain A.
  *
  * @tparam A The domain type.
  * @tparam B The value for an input from the domain.
  */
trait Distribution[A, B] extends PartialFunction[A, B] with Traversable[(A, B)] {

  override def apply(a: A): B = get(a).getOrElse(throw new NoSuchElementException(s"Value undefined for domain value $a"))

  override def isDefinedAt(a: A): Boolean = get(a).isDefined

  def get(a: A): Option[B]

  /**
    * Gets the domain list, sorted according to DomainOps.
    */
  def domain(implicit dops: DomainOps[A]): Seq[A] = this.map(_._1).toSeq.sorted(dops.ordering)

  /**
    * Gets the pairs, sorted according to DomainOps.
    */
  def pairs(implicit dops: DomainOps[A]): Seq[(A, B)] = this.toSeq.sortBy(_._1)(dops.ordering)

  /**
    * Returns the frequency as a simple Map[A, B]
    */
  def toMap: Map[A, B]

}

object Distribution {

  type FrequencyPair[A] = (A, Long)

  type ProbabilityPair[A] = (A, Probability)

}
