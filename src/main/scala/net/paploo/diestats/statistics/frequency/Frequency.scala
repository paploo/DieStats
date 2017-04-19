package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.{ConcreteDistributionCompanion, Distribution}
import net.paploo.diestats.statistics.pdf.{PDF, PDFAble}

import scala.language.implicitConversions

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
  def ++(pairs: Iterable[(A, Long)]): Frequency[A]

  /**
    * Give the sum total of counts across the domains.
    * @return
    */
  def count: Long

  override def toFrequency: Frequency[A] = this

  override def toPDF: PDF[A] = {
    val sum = this.count.toDouble
    val pairs = this.toMap.mapValues(m => Probability(m.toDouble / sum))
    PDF.buildFrom(pairs)
  }

}

object Frequency extends ConcreteDistributionCompanion[Long, Frequency] {

  override def empty[A]: Frequency[A] = FrequencyMap.empty

  override def buildFrom[A](pairs: Iterable[(A, Long)]): Frequency[A] = FrequencyMap.buildFrom(pairs)

  trait Implicits {
    implicit def frequencyToTraversable[A](frequency: Frequency[A]): Iterable[(A, Long)] = frequency.toSeq
    implicit def traversableToFrequency[A](traversable: Iterable[(A, Long)]): Frequency[A] = Frequency.buildFrom(traversable)
  }

  object Implicits extends Implicits

}

/**
  * A common trait for all values that can be transformed into a Frequency.
  */
trait Frequenciable[A] {

  def toFrequency: Frequency[A]

}