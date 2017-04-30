package net.paploo.diestats.statistics.util

/**
  * A typeclass interface to random number generation
  */
trait RandomGenerator {
  def nextProbability(): Probability
}

object RandomGenerator {

  implicit val default: RandomGenerator = new WrappedRandomRandomGenerator()

  def apply(): RandomGenerator = default

  def apply(seed: Long): RandomGenerator = new WrappedRandomRandomGenerator(seed)

  def apply(rand: scala.util.Random): RandomGenerator = new WrappedRandomRandomGenerator(rand)

  def apply(rand: java.util.Random): RandomGenerator = new WrappedRandomRandomGenerator(rand)

  /**
    * Creates a random generator with the iterable sequence of values.
    *
    * Can be used to generate values using a pre-made list of values, or even
    * by using an infinite stream.
    */
  def apply(iterable: Iterable[Probability]): RandomGenerator = apply(iterable.iterator)

  def apply(iterator: Iterator[Probability]): RandomGenerator = new RandomGenerator {
    override def nextProbability(): Probability = iterator.next()
  }

  /**
    * Creates a single-valued generator. This is most useful for testing.
    * @param value
    * @return
    */
  def apply(value: Probability): RandomGenerator = new RandomGenerator {
    override def nextProbability(): Probability = value
  }

  class WrappedRandomRandomGenerator(rand: scala.util.Random)    extends RandomGenerator {

    def this() = this(new scala.util.Random)

    def this(seed: Long) = this(new scala.util.Random(seed))

    def this(rand: java.util.Random) = this(new scala.util.Random(rand))

    /**
      * Gives a probability from 0 (inclusive) to 1 (exclusive).
      * @return
      */
    override def nextProbability(): Probability = Probability(rand.nextLong, java.lang.Long.MAX_VALUE)
  }

}

