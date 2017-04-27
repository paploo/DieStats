package net.paploo.diestats.statistics.frequency

/**
  * Imutable implementation of frequency, build around immutable Map.
  * @param frequencies The set of frequencies.
  * @tparam A The domain type.
  */
private[frequency] final class FrequencyMap[A](frequencies: Map[A, Long]) extends Frequency[A] {

  override def get(a: A): Option[Long] = frequencies.get(a)

  override lazy val sum: Long = frequencies.values.sum

  override lazy val size: Int = frequencies.size

  override def +(pair: (A, Long)): Frequency[A] =
    new FrequencyMap[A](frequencies.updated(
      pair._1,
      frequencies.getOrElse(pair._1, 0L) + pair._2
    ))

  override def ++(pairs: Iterable[(A, Long)]): Frequency[A] =
    pairs.foldLeft(this.toFrequency)(_ + _)

  override def toMap: Map[A, Long] = frequencies

  override def toString(): String = s"FrequencyMap(${this.toMap.mkString(", ")})"
}

object FrequencyMap extends FrequencyCompanion[FrequencyMap] {

  override def empty[A]: FrequencyMap[A] = new FrequencyMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Long)]): FrequencyMap[A] = new FrequencyMap(pairs.toMap)

  override def buildFromValues[A](values: Iterable[A]): FrequencyMap[A] = {
    val buffer = FrequencyBuffer.empty[A]
    values.foreach(buffer.append)
    buildFrom(buffer.toMap)
  }

}