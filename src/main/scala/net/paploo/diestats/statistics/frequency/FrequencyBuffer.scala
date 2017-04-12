package net.paploo.diestats.statistics.frequency

/**
  * Base trait of all mutable frequency buffers, defining methods that only affect mutable buffers.
  * @tparam A The domain type.
  */
trait FrequencyBuffer[A] extends Frequency[A] {

  def +=(pair: (A, Long)): FrequencyBuffer[A]

  def ++=(pairs: Traversable[(A, Long)]): FrequencyBuffer[A]

  def copy: FrequencyBuffer[A]

}

object FrequencyBuffer {

  def empty[A](): FrequencyBuffer[A] = AtomicFrequencyBuffer.empty[A]() //Establish the preferred form for frequency buffers

  def apply[A](): FrequencyBuffer[A] = empty()

  def apply[A](pairs: Traversable[(A, Long)]): FrequencyBuffer[A] = empty() ++= pairs

}