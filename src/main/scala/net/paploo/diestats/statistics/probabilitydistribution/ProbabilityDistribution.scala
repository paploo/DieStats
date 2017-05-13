package net.paploo.diestats.statistics.probabilitydistribution

import net.paploo.diestats.statistics.distribution.{Distribution, DistributionCompanion, DistributionStatistics, NumericDistributionStatistics, StatisticalDistribution}
import net.paploo.diestats.statistics.util.{FrequencyNumeric, Monoid, Probability}

import scala.collection.mutable
import scala.language.higherKinds

/**
  * Probabilitiy Distribution
  */
trait ProbabilityDistribution[A] extends Distribution[A, Probability] with StatisticalDistribution[A, Probability] with ProbabilityDistributionable[A] {

  // Technically, we only need a Semigroup[A] instead of a Monoid[A], but I'm attempting to avoid type-class explosion.
  def convolve(that: ProbabilityDistribution[A])(implicit monoid: Monoid[A]): ProbabilityDistribution[A] = {
    // For both better memory usage and speed, use mutable.Map as a buffer, and then make immutable.
    val buffer = mutable.Map.empty[A, Probability]
    for {
      (xa, xp) <- this.toSeq
      (ya, yp) <- that.toSeq
      key = monoid.concat(xa, ya)
      value = buffer.getOrElseUpdate(key, Probability.zero) + (xp * yp)
    } buffer += key -> value

    ProbabilityDistribution.buildFromNormalized(buffer) //Note: This is technically mutable, but we return as an immutable interface; we could use toMap to be safer, but then we'd be making an unecesssary copy to enforce immutability.
  }

  /**
    * Maps the domain into a new domain.
    *
    * Pairs that are identical are coalesced.
    */
  def mapDomain[B](f: A => B): ProbabilityDistribution[B] =
    ProbabilityDistribution.buildFromNormalized(
      ProbabilityDistribution.reduceNormalizedPairs(
        this.toSeq.map(p => f(p._1) -> p._2)
      )
    )

  override def toStatistics(implicit ord: Ordering[A]): DistributionStatistics[A, Probability] =
    DistributionStatistics.fromDistributionPairs(toSeq)

  override def toNumericalDomainStatistics(implicit num: Numeric[A]): NumericDistributionStatistics[A, Probability] =
    DistributionStatistics.fromNumericDistributionPairs(toSeq)
}

object ProbabilityDistribution extends ProbabilityDistributionCompanion[ProbabilityDistribution] {

  override private[probabilitydistribution] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): ProbabilityDistribution[A] =
    ProbabilityDistributionMap.buildFromNormalized(pairs)

}

/**
  * A common trait for all values that can be transformed into a ProbabilityDistribution.
  */
trait ProbabilityDistributionable[A] {

  def toProbabilityDistribution: ProbabilityDistribution[A]

}

trait ProbabilityDistributionCompanion[Repr[_]] extends DistributionCompanion[Probability, Repr] {

  override def empty[A]: Repr[A] = buildFrom(None)

  def normalize[A, N](pairs: (A, N)*)(implicit num: FrequencyNumeric[N]): Repr[A] = buildNormalizedFrom(pairs)

  /**
    * Construct from a given set of pairs, with auto-normalization of the pairs.
    */
  def buildNormalizedFrom[A, N](pairs: Iterable[(A, N)])(implicit num: FrequencyNumeric[N]): Repr[A] =
    buildFromNormalized(Probability.normalizePairs(pairs))

  /**
    * Given the pairs, create a ProbabilityDistribution.
    *
    * Distributions given to this method should be pre-normalized,
    * and will thrown an exception if they are not.
    */
  override def buildFrom[A](pairs: Iterable[(A, Probability)]): Repr[A] = {
    val sum = pairs.map(_._2).sum
    require(sum == Probability.one, s"Expected to build a normalized distribution but instead sums to $sum: $pairs")

    buildFromNormalized(Probability.normalizePairs(pairs))
  }

  /**
    * Create a ProbabilityDistribution from guaranteed pre-normalized pairs, skipping any validation of normalization.
    *
    * This is used with caution, to get performance increases in cases where we know we can trust the source.
    *
    * Visibility is kept to within the statistics package, where we can trust the caller has pre-normalized.
    */
  private[probabilitydistribution] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): Repr[A]

  /**
    * Reduces pairs over the domain, by adding the probabilities.
    *
    * This is used with caution, to get performance increases in cases where we know we can trust the source.
    *
    * Visibility is kept to within the statistics package, where we can trust the caller has pre-normalized.
    * @param pairs
    * @tparam A
    * @return
    */
  private[probabilitydistribution] def reduceNormalizedPairs[A](pairs: Iterable[(A, Probability)]): Iterable[(A, Probability)] =
    Distribution.reducePairs(pairs)

  /**
    * 1. Convolves each of the distributions into a new domain that is the sequence of the individual domains.
    * 2. Applies the given domain mapping operation to the convolved domain, producing a new domain.
    * 3. Reduces the result so that equivalent domain
    *
    * This can be used to implement operations like `best` and `worst` on a series of distributions.
    *
    * Note that `a.convolve(b)(m)` is logically identical to `mapConvolve(Seq(a,b))(m.reduce)`
    *
    * WARNING: The this method is O(k^n) on n as the number of distributions passsed, and k as the domain size.
    *
    * Best/Worst can be implemented from this:
    * 1. It Convolves eac of the distributions into a domain that is a sequence of the domain values,
    * 2. The mapping function can then select the best/worst n entries, and concatenate them monoidally, and
    * 3. It'll reduce the probabilities for each of these new domain entries.
    *
    * @param distributions
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def mapConvolve[A, B](distributions: Iterable[ProbabilityDistribution[A]])(f: Seq[A] => B): Repr[B] = {
    val initialMemo = Vector((Vector.empty[A], Probability.one))

    val convolved: Iterable[(Seq[A], Probability)] = distributions.foldLeft(initialMemo) { (memo, dist) =>
      for {
        (as, ps) <- memo
        (a, p) <- dist
      } yield (as :+ a, ps * p)
    }

    val mapped: Iterable[(B, Probability)] = convolved.map(p => f(p._1) -> p._2)

    buildFromNormalized(reduceNormalizedPairs(mapped))
  }

  def mapConvolve[A, B](distributions: ProbabilityDistribution[A]*)(f: Seq[A] => B): Repr[B] =
    mapConvolve(distributions)(f)

}
