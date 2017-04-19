package net.paploo.diestats.statistics.domain

import net.paploo.diestats.statistics.util.Monoid

import scala.annotation.implicitNotFound

/**
  * A typeclass for defining a domain over A.
  *
  * Example Domains
  *
  * Classical Dice Games:
  * You roll 2d6 and look at the sum.
  * Each die has domain values 1-6 and are concatenated via addition, giving us
  * the roll domain values of 2-12.
  * Thus we use the standard Monoid[Int] and Ordering[Int] to represent our domain.
  *
  * Counting:
  * You roll 4 dice, each with sides labelled either "A", "B", or "C"; our outcomes
  * are determined by the quantity of "A", "B", and "C" sides.
  * While each die result is only the String label, we are interested in the
  * unordered sequence of strings of the rolls, e.g. {"A", "B", "B", "D"}.
  * Thus, assuming we have an UnorderedList[A] container that can keep multiple
  * elements, but cares not for ordering, our domain is UnorderedList[String]
  * with contatenation being simple list concatenation.
  * Additionally, since ops comparison comes via the DomainOps, we can use a
  * standard Seq to represent the domain, and build the unordered comparisons
  * into the the Ordering; thus using a Seq[A].
  *
  */
@implicitNotFound(msg = "Cannot find an implicit DomainOps[${A}].")
trait DomainOps[A] extends Ordering[A] with Monoid[A]

object DomainOps {

  class StringDomainOps extends DomainOps[String] {
    override def concat(x: String, y: String): String = x + y

    override val empty: String = ""

    override def compare(x: String, y: String): Int = x compare y
  }

  class NumericDomainOps[A](implicit val num: Numeric[A]) extends DomainOps[A] {

    override def concat(x: A, y: A): A = num.plus(x, y)

    override def empty: A = num.zero

    override def compare(x: A, y: A): Int = num.compare(x, y)


  class UnorderedSeqDomainOps[A](implicit val ordering: Ordering[A]) extends DomainOps[Seq[A]] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

    override def empty: Seq[A] = Seq.empty

    /**
      * The only requirement of the primary use case is that two sequences are considered
      * equal if they contain the same elements, even if their ordering is different.
      *
      * This requirement is met if we produce sorted the sets, and then compare their
      * elements using the Ordering[A], using the same rules as String sorting of Character.
      */
    override def compare(x: Seq[A], y: Seq[A]): Int = {
      val as = x.sorted.iterator
      val bs = y.sorted.iterator

      // Find the first non-matching element.
      while(as.hasNext && bs.hasNext) {
        val cmp = ordering.compare(as.next(), bs.next())
        if (cmp != 0) return cmp
      }

      // If we got here, they were the same up until one ran out of elements, so it just comes down to size.
      return as.length - bs.length
    }
  }

  trait Implicits {
    implicit val stringDomainOps: DomainOps[String] = new StringDomainOps
    implicit def numericDomainOps[A](implicit num: Numeric[A]): DomainOps[A] = new NumericDomainOps[A]
  }

  object Implicits extends Implicits

}