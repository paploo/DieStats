package net.paploo.diestats.statistics.frequency
import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion

class FrequencyMap[A](frequencies: Map[A, Long]) extends Frequency[A] {

  override def +(pair: (A, Long)): Frequency[A] = FrequencyMap.buildFrom(frequencies + pair)

  override def ++(pairs: Iterable[(A, Long)]): Frequency[A] = FrequencyMap.buildFrom(frequencies ++ pairs)

  override def get(a: A): Option[Long] = frequencies.get(a)

  lazy val count: Long = frequencies.values.sum

  override def toMap: Map[A, Long] = frequencies

}

object FrequencyMap extends ConcreteDistributionCompanion[Long, FrequencyMap] {

  override def empty[A]: FrequencyMap[A] = new FrequencyMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyMap[A] = new FrequencyMap(pairs.toMap)

}