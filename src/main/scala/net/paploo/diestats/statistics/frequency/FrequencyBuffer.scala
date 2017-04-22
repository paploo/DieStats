package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion

/**
  * Base trait of all mutable frequency buffers, defining methods that only affect mutable buffers.
  *
  * @tparam A The domain type.
  */
trait FrequencyBuffer[A] extends Frequency[A] {

  /**
    * Mutably appends the given counts.
    * @param pair
    * @return
    */
  def +=(pair: (A, Long)): FrequencyBuffer[A]

  /**
    * Mutably appends the given counts.
    * @param pairs
    * @return
    */
  def ++=(pairs: Iterable[(A, Long)]): FrequencyBuffer[A]

  /**
    * Mutably a value with a count of 1.
    * @param value
    * @return
    */
  def append(value: A): Frequency[A] = this += ((value, 1L))

  /**
    * Mutably appends the list of values, with a frequency count for each instance in the list.
    * @param values
    * @return
    */
  def append(values: Iterable[A]): Frequency[A] = this ++= values.map((_,1L))

  def copy: FrequencyBuffer[A]

}

object FrequencyBuffer extends ConcreteDistributionCompanion[Long, FrequencyBuffer] {

  override def empty[A]: FrequencyBuffer[A] = AtomicFrequencyBuffer.empty[A]

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyBuffer[A] = empty ++= pairs

}