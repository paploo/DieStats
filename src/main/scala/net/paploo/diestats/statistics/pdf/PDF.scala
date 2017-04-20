package net.paploo.diestats.statistics.pdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.{ConcreteDistributionCompanion, ProbabilityDistribution}
import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.Implicits._
import net.paploo.diestats.statistics.cdf.CDFAble
import net.paploo.diestats.statistics.frequency.Frequency

import scala.collection.mutable

/**
  * Probabilitiy Distribution Function
  */
trait PDF[A] extends ProbabilityDistribution[A] with PDFAble[A] with CDFAble[A] {

  def convolve(that: PDF[A])(implicit dops: DomainOps[A]): PDF[A] = {
    // For both better memory usage and speed, use mutable.Map as a buffer, and then make immutable.
    val buffer = mutable.Map.empty[A, Probability]
    for {
      (xa, xp) <- this.toSeq
      (ya, yp) <- that.toSeq
    } {
      val key = dops.concat(xa, ya)
      val value = buffer.getOrElseUpdate(key, Probability.zero) + (xp * yp)
      buffer += key -> value
    }
    PDF.buildFromNormalized(buffer) //Note: This is technically mutable, but we return as an immutable interface; we could use toMap to be safer, but then we'd be making an unecesssary copy to enforce immutability.
  }

}

object PDF extends ConcreteDistributionCompanion[Probability, PDF] {

  override def empty[A]: PDF[A] = PDFMap.empty

  def apply[A](freq: Frequency[A]): PDF[A] = {
    val sum = freq.count.toDouble
    val pairs = freq.toMap.mapValues(m => Probability(m.toDouble / sum))
    PDF.buildFrom(pairs)
  }

  /**
    * Given the pairs, create a PDF.
    *
    * This normalizes any unnormalized distributions.
    */
  override def buildFrom[A](pairs: Iterable[(A, Probability)]): PDF[A] = {
    val normalizedPairs = Probability.normalizePairs(pairs)
    PDFMap.buildFrom(normalizedPairs)
  }

  /**
    * Create a PDF from guaranteed pre-normalized pairs, skipping any validation of normalization.
    *
    * This is used with caution, to get performance increases in cases where we know we can trust the source.
    *
    * Visibility is kept to within the statistics package, where we can trust the caller has pre-normalized.
    */
  private[pdf] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): PDF[A] =
    PDFMap.buildFromNormalized(pairs)

}

/**
  * A common trait for all values that can be transformed into a PDF.
  */
trait PDFAble[A] {

  def toPDF: PDF[A]

}
