package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion

/**
  * Base trait of all mutable frequency buffers, defining methods that only affect mutable buffers.
  *
  * @tparam A The domain type.
  */
trait FrequencyBuffer[A] extends Frequency[A] {

  def +=(pair: (A, Long)): FrequencyBuffer[A]

  def ++=(pairs: Iterable[(A, Long)]): FrequencyBuffer[A]

  def copy: FrequencyBuffer[A]

}

object FrequencyBuffer extends ConcreteDistributionCompanion[Long, FrequencyBuffer] {

  override def empty[A]: FrequencyBuffer[A] = AtomicFrequencyBuffer.empty[A]

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyBuffer[A] = empty ++= pairs

}