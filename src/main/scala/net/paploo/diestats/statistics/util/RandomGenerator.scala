package net.paploo.diestats.statistics.util

/**
  * A typeclass interface to random number generation
  */
trait RandomGenerator {
  def nextProbability(): Probability
}

object RandomGenerator {

  def apply(): RandomGenerator = new WrappedRandomRandomGenerator()

  def apply(seed: Long): RandomGenerator = new WrappedRandomRandomGenerator(seed)

  def apply(rand: scala.util.Random): RandomGenerator = new WrappedRandomRandomGenerator(rand)

  def apply(rand: java.util.Random): RandomGenerator = new WrappedRandomRandomGenerator(rand)

  /**
    * Creates a random generator with the iterable sequence of values.
    *
    * Can be used to generate values using a pre-made list of values, or event
    * by using an infinite stream.
    */
  def apply(iterable: Iterable[Probability]): RandomGenerator = new WrappedIteraterRandomGenerator(iterable.iterator)

  def apply(iterator: Iterator[Probability]): RandomGenerator = new WrappedIteraterRandomGenerator(iterator)

  private[this] class WrappedRandomRandomGenerator(rand: scala.util.Random) extends RandomGenerator {

    def this() = this(new scala.util.Random)

    def this(seed: Long) = this(new scala.util.Random(seed))

    def this(rand: java.util.Random) = this(new scala.util.Random(rand))

    override def nextProbability(): Probability = Probability(rand.nextDouble())

  }

  private[this] class WrappedIteraterRandomGenerator(it: Iterator[Probability]) extends RandomGenerator {
    override def nextProbability(): Probability = it.next()
  }

}

