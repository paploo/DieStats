package net.paploo.diestats.statistics.frequency

/**
  * Base trait of all mutable frequency buffers, defining methods that only affect mutable buffers.
  * @tparam A The domain type.
  */
trait FrequencyBuffer[A] extends Frequency[A] {

  def +=(pair: (A, Long)): FrequencyBuffer[A]

  def ++=(that: Frequency[A]): FrequencyBuffer[A]

}