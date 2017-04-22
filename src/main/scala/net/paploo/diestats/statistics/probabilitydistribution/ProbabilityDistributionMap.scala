package net.paploo.diestats.statistics.probabilitydistribution
import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion
import net.paploo.diestats.statistics.util.Probability

/**
  * Implementation of ProbabilityDistribution built around immutable Map.
  * @param normalizedPairs A pre-normalized set of probabilities over the domain.
  * @tparam A
  */
private[probabilitydistribution] final class ProbabilityDistributionMap[A](normalizedPairs: Map[A, Probability]) extends ProbabilityDistribution[A] {

  override def get(a: A): Option[Probability] = normalizedPairs.get(a)

  override def toMap: Map[A, Probability] = normalizedPairs

  override def toProbabilityDistribution: ProbabilityDistribution[A] = this

}

object ProbabilityDistributionMap extends ConcreteDistributionCompanion[Probability, ProbabilityDistributionMap] {

  override def empty[A]: ProbabilityDistributionMap[A] = new ProbabilityDistributionMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Probability)]): ProbabilityDistributionMap[A] = new ProbabilityDistributionMap(pairs.toMap)

  private[probabilitydistribution] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): ProbabilityDistributionMap[A] = new ProbabilityDistributionMap(pairs.toMap)

}
