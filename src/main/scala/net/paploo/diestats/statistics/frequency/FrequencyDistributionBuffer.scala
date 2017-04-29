package net.paploo.diestats.statistics.frequency

/**
  * Base trait of all mutable frequency buffers, defining methods that only affect mutable buffers.
  *
  * @tparam A The domain type.
  */
trait FrequencyDistributionBuffer[A] extends FrequencyDistribution[A] {

  /**
    * Mutably appends the given counts.
    * @param pair
    * @return
    */
  def +=(pair: (A, Long)): FrequencyDistributionBuffer[A]

  /**
    * Mutably appends the given counts.
    * @param pairs
    * @return
    */
  def ++=(pairs: Iterable[(A, Long)]): FrequencyDistributionBuffer[A]

  def :+=(value: A): FrequencyDistribution[A] = this += ((value, 1L))

  def :++=(values: Iterable[A]): FrequencyDistribution[A] = this ++= values.map((_,1L))

  /**
    * Mutably a value with a count of 1.
    * @param value
    * @return
    */
  def append(value: A): FrequencyDistribution[A] = this += ((value, 1L))

  /**
    * Mutably appends the list of values, with a frequency count for each instance in the list.
    * @param values
    * @return
    */
  def append(values: Iterable[A]): FrequencyDistribution[A] = this ++= values.map((_,1L))

  def copy: FrequencyDistributionBuffer[A]

}

object FrequencyDistributionBuffer extends FrequencyDistributionCompanion[FrequencyDistributionBuffer] {

  override def empty[A]: FrequencyDistributionBuffer[A] = AtomicFrequencyDistributionBuffer.empty[A]

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyDistributionBuffer[A] = empty ++= pairs

  override def buildFromValues[A](values: Iterable[A]): FrequencyDistributionBuffer[A] = {
    val buffer = FrequencyDistributionBuffer.empty[A]
    values.foreach(buffer.append)
    buffer
  }

}