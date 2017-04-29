package net.paploo.diestats.statistics.frequency

/**
  * Imutable implementation of frequency, build around immutable Map.
  * @param frequencies The set of frequencies.
  * @tparam A The domain type.
  */
private[frequency] final class FrequencyDistributionMap[A](frequencies: Map[A, Long]) extends FrequencyDistribution[A] {

  override def get(a: A): Option[Long] = frequencies.get(a)

  override lazy val sum: Long = frequencies.values.sum

  override lazy val size: Int = frequencies.size

  override def +(pair: (A, Long)): FrequencyDistribution[A] =
    new FrequencyDistributionMap[A](frequencies.updated(
      pair._1,
      frequencies.getOrElse(pair._1, 0L) + pair._2
    ))

  override def ++(pairs: Iterable[(A, Long)]): FrequencyDistribution[A] =
    pairs.foldLeft(this.toFrequency)(_ + _)

  override def toMap: Map[A, Long] = frequencies

  override def toString(): String = s"FrequencyMap(${this.toMap.mkString(", ")})"
}

object FrequencyDistributionMap extends FrequencyDistributionCompanion[FrequencyDistributionMap] {

  override def empty[A]: FrequencyDistributionMap[A] = new FrequencyDistributionMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyDistributionMap[A] = new FrequencyDistributionMap(pairs.toMap)

  override def buildFromValues[A](values: Iterable[A]): FrequencyDistributionMap[A] = {
    val buffer = FrequencyDistributionBuffer.empty[A]
    values.foreach(buffer.append)
    buildFrom(buffer.toMap)
  }

}