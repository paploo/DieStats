package net.paploo.diestats.statistics.domain

import scala.annotation.implicitNotFound

/**
  * A typeclass for defining a domain over A.
  */
@implicitNotFound(msg = "Cannot find an implicit DomainOps[${A}].")
trait DomainOps[A] {

  /**
    * Concatenation, usually in the Monoidal sense.
    */
  def ++(a: A, b: A): A

  def empty: A

  def ordering: Ordering[A]
}