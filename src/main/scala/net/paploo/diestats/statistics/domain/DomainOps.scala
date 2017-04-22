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

  def apply[A](ord: Ordering[A], monoid: Monoid[A]): DomainOps[A] =
    new PiecewiseDomainOps[A](ord, monoid)

  class PiecewiseDomainOps[A](ord: Ordering[A], monoid: Monoid[A]) extends DomainOps[A] {
    override def concat(x: A, y: A): A = monoid.concat(x ,y)

    override val empty: A = monoid.empty

    override def compare(x: A, y: A): Int = ord.compare(x, y)
  }

  class StringDomainOps extends PiecewiseDomainOps[String](Ordering.String, Monoid.stringMonoid)
  val stringDomainOps: DomainOps[String] = new StringDomainOps()


  class AdditiveDomainOps[N](implicit num: Numeric[N]) extends PiecewiseDomainOps[N](num, Monoid.additiveMonoid(num))
  def additiveDomainOps[N](implicit num: Numeric[N]): DomainOps[N] = new AdditiveDomainOps[N]()

  class MultiplicativeDomainOps[N](implicit num: Numeric[N]) extends PiecewiseDomainOps[N](num, Monoid.multiplicativeMonoid(num))
  def multiplicativeDomainOps[N](implicit num: Numeric[N]): DomainOps[N] = new MultiplicativeDomainOps[N]()

  class OrderedSeqDomainOps[A](implicit val ordering: Ordering[A]) extends DomainOps[Seq[A]] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = (x ++ y).sorted

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
      as.length - bs.length
    }
  }

  trait Implicits {
    implicit val stringDomainOps: DomainOps[String] = DomainOps.stringDomainOps

    /**
      * There are two definitions of Monoid over numerics, however addition is the one most used
      * with PDFs, so we default to this in the Implicits.
      */
    implicit def additiveDomainOps[A](implicit num: Numeric[A]): DomainOps[A] = new AdditiveDomainOps[A]
  }

  object Implicits extends Implicits

}