package net.paploo.diestats.statistics.cdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.util.RandomGenerator

private[cdf] class CDFMap[A](validatedPairs: Map[A, Probability])(implicit ordering: Ordering[A]) extends CDF[A] {

  override def domainOrdering: Ordering[A] = ordering

  override def get(a: A): Option[Probability] = validatedPairs.get(a)

  /**
    * Returns a random domain value, distributed according to the CDF.
    */
  override def randomValue(implicit rand: RandomGenerator): A = ???

  override def toMap: Map[A, Probability] = validatedPairs

}

object CDFMap {

  def empty[A](implicit ordering: Ordering[A]): CDFMap[A] = new CDFMap[A](Map.empty)

  private[cdf] def buildFromValidatedPairs[A](pairs: Iterable[(A, Probability)])(implicit ordering: Ordering[A]): CDFMap[A] = new CDFMap(pairs.toMap)

}
