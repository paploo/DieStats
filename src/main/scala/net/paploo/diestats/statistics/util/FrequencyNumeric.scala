package net.paploo.diestats.statistics.util

import scala.math.{Ordering => ScalaOrdering}

/**
  * Trait for handling the numeric for the "frequencies" of a distribution.
  *
  * This slightly augments the standard Numeric[N] typeclass with
  * probability normalization methods.
  * @tparam N
  */
@annotation.implicitNotFound(msg = "No implicit FrequencyNumeric defined for ${N}.")
trait FrequencyNumeric[N] extends Numeric[N] {
  /**
    * Creates a probability from a numerator and denominator of type N.
    *
    * Effectively normalizes the numerator and denominator.
    *
    * For integral types, this retain perfect accuracy; for fractional types it
    * may have accuracy loss, depending on how the value is stored.
    * @param numerator
    * @param denominator
    * @return
    */
  def toProbability(numerator: N, denominator: N): Probability
}

object FrequencyNumeric {

  /**
    * Implementation for Long.
    *
    * This is typically used on traditional frequency counts.
    */
  trait LongFrequencyNumeric extends FrequencyNumeric[Long] with Numeric.LongIsIntegral with ScalaOrdering.LongOrdering {
    override def toProbability(numerator: Long, denominator: Long): Probability = Probability(numerator, denominator)
  }
  implicit object LongFrequencyNumeric extends LongFrequencyNumeric

  trait BigIntFrequencyNumeric extends FrequencyNumeric[BigInt] with Numeric.BigIntIsIntegral with ScalaOrdering.BigIntOrdering {
    override def toProbability(numerator: BigInt, denominator: BigInt): Probability = Probability(numerator, denominator)
  }
  implicit object BigIntFrequencyNumeric extends BigIntFrequencyNumeric

  /**
    * Implementation for Probaility.
    *
    * Normalized distributions (e.g. probability density distributions) usually use Probability as the frequency.
    */
  trait ProbabilityFrequencyNumeric extends FrequencyNumeric[Probability] with Probability.ProbabilityIsFractional with Probability.ProbabilityOrdering {
    override def toProbability(numerator: Probability, denominator: Probability): Probability = numerator / denominator
  }
  implicit object ProbabilityFrequencyNumeric extends ProbabilityFrequencyNumeric

  trait DoubleFrequencyNumeric extends FrequencyNumeric[Double] with Numeric.DoubleIsFractional with ScalaOrdering.DoubleOrdering {
    override def toProbability(numerator: Double, denominator: Double): Probability = Probability(numerator/denominator)
  }
  implicit object DoubleFrequencyNumeric extends DoubleFrequencyNumeric

}
