package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.Distribution
import net.paploo.diestats.statistics.distribution.Distribution.FrequencyPair
import net.paploo.diestats.statistics.pdf.PDFAble
import net.paploo.diestats.util.TraversableSupport

import scala.collection.{TraversableLike, mutable}

/**
  * Frequency base trait, defining all methods that can be used by both immutable and mutable subclasses.
  * @tparam A The domain type.
  */
trait Frequency[A] extends Distribution[A, Long] with TraversableLike[A, Frequency[A]] with Frequenciable[A] with PDFAble[A] {

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

  //override protected[this] def newBuilder: mutable.Builder[(A, Long), Traversable[(A, Long)]] = Frequency.builder
}

object Frequency {

  def empty[A]: Frequency[A] = FrequencyMap.empty

  def apply[A](): Frequency[A] = empty

  def apply[A](pairs: Traversable[(FrequencyPair[A]]): Frequency[A] = FrequencyMap(pairs)

  //def newBuilder[A]: mutable.Builder[FrequencyPair[A], Traversable[FrequencyPair[A]]] = TraversableSupport.SequenceBufferBuilder.apply[FrequencyPair[A], Frequency[A]](Frequency.apply)

}

/**
  * A common trait for all values that can be transformed into a Frequency.
  */
trait Frequenciable[A] {

  def toFrequency: Frequency[A]

}
