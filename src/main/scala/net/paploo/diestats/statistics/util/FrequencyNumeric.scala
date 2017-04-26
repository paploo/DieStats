package net.paploo.diestats.statistics.util

import scala.math.{Ordering => ScalaOrdering}

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

  trait LongFrequencyNumeric extends FrequencyNumeric[Long] with Numeric.LongIsIntegral with ScalaOrdering.LongOrdering {
    override def toProbability(numerator: Long, denominator: Long): Probability = Probability(numerator, denominator)
  }
  implicit object LongFrequencyNumeric extends LongFrequencyNumeric

  trait BigIntFrequencyNumeric extends FrequencyNumeric[BigInt] with Numeric.BigIntIsIntegral with ScalaOrdering.BigIntOrdering {
    override def toProbability(numerator: BigInt, denominator: BigInt): Probability = Probability(numerator, denominator)
  }
  implicit object BigIntFrequencyNumeric extends BigIntFrequencyNumeric

  trait ProbabilityFrequencyNumeric extends FrequencyNumeric[Probability] with Probability.ProbabilityIsFractional with Probability.ProbabilityOrdering {
    override def toProbability(numerator: Probability, denominator: Probability): Probability = numerator / denominator
  }
  implicit object ProbabilityFrequencyNumeric extends ProbabilityFrequencyNumeric

//  trait Implicits {
//    implicit val longFrequencyNumeric: FrequencyNumeric[Long] = LongFrequencyNumeric
//    implicit val bigIntFrequencyNumeric: FrequencyNumeric[BigInt] = BigIntFrequencyNumeric
//    implicit val probabilityFrequencyNumeric: FrequencyNumeric[Probability] = ProbabilityFrequencyNumeric
//  }
//  object Implicits extends Implicits

}
