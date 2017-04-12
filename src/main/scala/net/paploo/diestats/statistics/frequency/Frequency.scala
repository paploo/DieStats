package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.Distribution
import net.paploo.diestats.statistics.pdf.PDFAble

/**
  * Frequency base trait, defining all methods that can be used by both immutable and mutable subclasses.
  * @tparam A The domain type.
  */
trait Frequency[A] extends Distribution[A, Long] with Frequenciable[A] with PDFAble[A] {

  /**
    * Returns a new Frequency with the given count added to the pair.
    * @param pair
    * @return
    */
  def +(pair: (A, Long)): Frequency[A]

  /**
    * Returns a new Frequency with the given frequencies added in.
    * @param pairs
    */
  def ++(pairs: Traversable[(A, Long)]): Frequency[A]

  /**
    * Give the sum total of counts across the domains.
    * @return
    */
  def count: Long

}

object Frequency {

  def empty[A]: Frequency[A] = FrequencyMap.empty

  def apply[A](): Frequency[A] = empty

  def apply[A](pairs: Traversable[(A, Long)]): Frequency[A] = FrequencyMap(pairs)

}

/**
  * A common trait for all values that can be transformed into a Frequency.
  */
trait Frequenciable[A] {

  def toFrequency: Frequency[A]

}
