package net.paploo.diestats.statistics.pdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.{ConcreteDistributionCompanion, ProbabilityDistribution}
import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.Implicits._

/**
  * Probabilitiy Distribution Function
  */
trait PDF[A] extends ProbabilityDistribution[A] {

  def convolve(that: PDF[A])(implicit dops: DomainOps[A]): PDF[A]

}

object PDF extends ConcreteDistributionCompanion[Probability, PDF] {

  override def empty[A]: PDF[A] = ???

  /**
    * Given the pairs, create a PDF.
    *
    * This normalizes any unnormalized distributions.
    */
  override def buildFrom[A](pairs: TraversableOnce[(A, Probability)]): PDF[A] = {
    val normalizedPairs = Probability.normalizePairs(pairs)
    ???
  }

  /**
    * Create a PDF from pre-normalized pairs.
    *
    * Visibility is kept to within the statistics package, where we can trust the input is pre-normalized.
    */
  private[statistics] def buildFromNormalized[A](pairs: TraversableOnce[(A, Probability)]): PDF[A] = ???

}

/**
  * A common trait for all values that can be transformed into a PDF.
  */
trait PDFAble[A] {

  def toPDF: PDF[A]

}
