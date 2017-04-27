package net.paploo.diestats.statistics.frequency

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

  def :+=(value: A): Frequency[A] = this += ((value, 1L))

  def :++=(values: Iterable[A]): Frequency[A] = this ++= values.map((_,1L))

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

object FrequencyBuffer extends FrequencyCompanion[FrequencyBuffer] {

  override def empty[A]: FrequencyBuffer[A] = AtomicFrequencyBuffer.empty[A]

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyBuffer[A] = empty ++= pairs

  override def buildFromValues[A](values: Iterable[A]): FrequencyBuffer[A] = {
    val buffer = FrequencyBuffer.empty[A]
    values.foreach(buffer.append)
    buffer
  }

}