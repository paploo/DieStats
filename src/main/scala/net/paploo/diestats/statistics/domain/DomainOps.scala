package net.paploo.diestats.statistics.domain

/**
  * A typeclass for defining a domain over A.
  */
trait DomainOps[A] {

  /**
    * Concatenation, usually in the Monoidal sense.
    */
  def ++(a: A, b: A): A

  def empty: A

  def ordering: Ordering[A]
}