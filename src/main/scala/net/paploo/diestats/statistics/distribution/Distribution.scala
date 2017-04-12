package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.domain.DomainOps

/**
  * The base trait for distributions over the domain A.
  *
  * @tparam A The domain type.
  * @tparam B The value for an input from the domain.
  */
trait Distribution[A, B] extends PartialFunction[A, B] {

  override def apply(a: A): B = get(a).getOrElse(throw new NoSuchElementException(s"Value undefined for domain value $a"))

  override def isDefinedAt(a: A): Boolean = get(a).isDefined

  def get(a: A): Option[B]

  def domain(implicit dops: DomainOps[A]): Seq[A]

  def pairs(implicit dops: DomainOps[A]): Seq[(A, B)]

}
