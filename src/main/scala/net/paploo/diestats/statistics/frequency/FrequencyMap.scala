package net.paploo.diestats.statistics.frequency
import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion

/**
  * Imutable implementation of frequency, build around immutable Map.
  * @param frequencies The set of frequencies.
  * @tparam A The domain type.
  */
private[frequency] final class FrequencyMap[A](frequencies: Map[A, Long]) extends Frequency[A] {

  override def +(pair: (A, Long)): Frequency[A] =
    new FrequencyMap[A](frequencies.updated(
      pair._1,
      frequencies.getOrElse(pair._1, 0L) + pair._2
    ))

  override def ++(pairs: Iterable[(A, Long)]): Frequency[A] =
    pairs.foldLeft(this.toFrequency)(_ + _)

  override def get(a: A): Option[Long] = frequencies.get(a)

  lazy val count: Long = frequencies.values.sum

  override def toMap: Map[A, Long] = frequencies

}

object FrequencyMap extends ConcreteDistributionCompanion[Long, FrequencyMap] {

  override def empty[A]: FrequencyMap[A] = new FrequencyMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyMap[A] = new FrequencyMap(pairs.toMap)

}