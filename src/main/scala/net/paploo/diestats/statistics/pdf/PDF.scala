package net.paploo.diestats.statistics.pdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.{ConcreteDistributionCompanion, ProbabilityDistribution}
import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.Implicits._

import scala.collection.mutable

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
  override def buildFrom[A](pairs: Iterable[(A, Probability)]): PDF[A] = {
    val normalizedPairs = Probability.normalizePairs(pairs)
    ???
  }

  /**
    * Create a PDF from pre-normalized pairs.
    *
    * Visibility is kept to within the statistics package, where we can trust the input is pre-normalized.
    */
  private[statistics] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): PDF[A] = ???

  //def convolve[A, B](a: PDF[A], b: PDF[A])(f: Map[A, Probability] => B)

  def convolveFunctional[A](xs: Seq[(A, Probability)], ys: Seq[(A, Probability)])(implicit dops: DomainOps[A]): Map[A, Probability] = {
    // First, we combine the pairs.
    val combinedPairs = for {
      (xa, xp) <- xs
      (ya, yp) <- ys
    } yield (dops.concat(xa, ya), xp * yp)

    //Now, we flatten together.
    val flattenedPairs = combinedPairs.groupBy(_._1).map {
      case (a, pairs) => a -> pairs.foldLeft(Probability.zero)(_ + _._2)
    }

    flattenedPairs
  }

  def convolveMutable[A](xs: Seq[(A, Probability)], ys: Seq[(A, Probability)])(implicit dops: DomainOps[A]): Map[A, Probability] = {
    val buffer = mutable.Map.empty[A, Probability]
    for {
      (xa, xp) <- xs
      (ya, yp) <- ys
    } {
      val key = dops.concat(xa, ya)
      val value = buffer.getOrElseUpdate(key, Probability.zero) + (xp * yp)
      buffer += key -> value
    }
    buffer.toMap
  }

}

/**
  * A common trait for all values that can be transformed into a PDF.
  */
trait PDFAble[A] {

  def toPDF: PDF[A]

}
