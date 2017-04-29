package net.paploo.diestats.statistics.probabilitydistribution
import net.paploo.diestats.statistics.util.Probability

/**
  * Implementation of ProbabilityDistribution built around immutable Map.
  * @param normalizedPairs A pre-normalized set of probabilities over the domain.
  * @tparam A
  */
private[probabilitydistribution] final class ProbabilityDistributionMap[A](normalizedPairs: Map[A, Probability]) extends ProbabilityDistribution[A] {

  override def get(a: A): Option[Probability] = normalizedPairs.get(a)

  override val size: Int = normalizedPairs.size

  override def toMap: Map[A, Probability] = normalizedPairs

  override def toProbabilityDistribution: ProbabilityDistribution[A] = this

  override def toString(): String = s"ProbabilityDistributionMap(${this.toMap.mkString(", ")})"
}

object ProbabilityDistributionMap extends ProbabilityDistributionCompanion[ProbabilityDistributionMap] {

  override private[probabilitydistribution] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): ProbabilityDistributionMap[A] = new ProbabilityDistributionMap(pairs.toMap)

}
