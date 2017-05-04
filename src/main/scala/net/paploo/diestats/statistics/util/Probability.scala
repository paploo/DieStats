package net.paploo.diestats.statistics.util

import scala.collection.immutable.ListMap

/**
  * Numeric class for representing Probabilities, which must live in the bounds [0,1].
  *
  * To give the guarantees that rounding errors do not occur, Probability is implemented
  * as a rational, with a numerator and denominator.
  *
  * Public constructors and operations guarantee that the resulting rational is
  * in reduced form.
  *
  * Probability seeks to guarantee freedom from rounding errors during computations
  * (which is especially important with the [0,1] bounds), and is guaranteed precise
  * when constructed with a rational number; however construction from inexact decimal
  * representations (e.g. Double) can lead to losses in precision.
  *
  * The implementation of this guarantee can change; however, in the current iteration, we represent
  * probabilities with rational values.
  *
  * TODO: Create a proper rational type and have Probability wrap around it.
  */
case class Probability private[Probability] (numerator: BigInt, denominator: BigInt) extends Ordered[Probability] {
  require(numerator >= 0L && denominator > 0L && numerator <= denominator, s"Probabilities must be in range [0,1], but got $this")

  def +(that: Probability): Probability = {
    // (n1/d1) + (n2/d2) = (n1*d2 + n2*d1) / (d1*d2)
    val numerator = (this.numerator * that.denominator) + (this.denominator * that.numerator)
    val denominator = this.denominator * that.denominator
    Probability(numerator, denominator)
  }

  def *(that: Probability): Probability = {
    // (n1*n2) / (d1*d2))
    val numerator = this.numerator * that.numerator
    val denominator = this.denominator * that.denominator
    Probability(numerator, denominator)
  }

  def -(that: Probability): Probability = {
    // (n1/d1) - (n2/d2) = (n1*d2 - n2*d1) / (d1*d2)
    val numerator = (this.numerator * that.denominator) - (this.denominator * that.numerator)
    val denominator = this.denominator * that.denominator
    Probability(numerator, denominator)
  }

  def /(that: Probability): Probability = {
    // (n1*d2) / (d1*n2))
    val numerator = this.numerator * that.denominator
    val denominator = this.denominator * that.numerator
    Probability(numerator, denominator)
  }

  def compliment: Probability = Probability.one - this

  override def compare(that: Probability): Int = {
    //Convert each numerator into a common denominator of (this.denominator * that.denominator) and compare
    val thisCommonNumerator = this.numerator * that.denominator
    val thatCommonNumerator = that.numerator * this.denominator

    thisCommonNumerator compare thatCommonNumerator
  }

  lazy val toDouble: Double = numerator.toDouble / denominator.toDouble

  lazy val toBigDecimal: BigDecimal = BigDecimal(numerator) / BigDecimal(denominator)

  override def equals(obj: Any): Boolean = obj match {
    case that: Probability => this.numerator == that.numerator && this.denominator == that.denominator
    case that: BigDecimal => this.toBigDecimal == that
    case that: Double => this.toDouble == that
    case that: Float => this.toDouble == that.toDouble
    case _ => false
  }

}

object Probability {

  /**
    * Gives back a Probability that is approximately equivalent to the passed Double value.
    * @param value
    * @return
    */
  def apply(value: Double): Probability = {
    require(value >= 0.0 && value <= 1.0, s"Probabilities must be in range [0,1], but got $value")
    /*
     * There are several techniques that can be used here:
     * * Multiply by a magic denominator, round, and that gives the numerator,
     * * Take the string output format, and use the digits as the numerator over that many digits over 10, and
     * * Process the IEEE bit representation to generate the exact base-2 numerator from the significand, and use the required base-2 denominator, to generate an exact value.
     */
    val numerator = (BigDecimal(value) * BigDecimal(doubleConversionDenominator)).rounded.toBigInt
    val denominator = BigInt(doubleConversionDenominator)
    apply(numerator, denominator)
  }

  //private[this] val doubleConversionDenominator = 4503599627370496L // 2**52, the number of bits in the IEEE double significand
  private[this] val doubleConversionDenominator = 6064949221531200L //Use the largest highly composite number I can find, in hopes of getting meaningful reductions sometimes.

  /**
    * Construct a reduced fraction Probability given a numerator and denominator.
    * @param numerator
    * @param denominator
    * @return The probability, as a reduced fraction.
    */
  def apply(numerator: BigInt, denominator: BigInt): Probability = {
    val gcd = numerator gcd denominator
    require(gcd != 0, s"Cannot construct probability for Probability($numerator, $denominator)")
    new Probability(numerator / gcd, denominator / gcd) //This is the root creation point, and calls new to do it.
  }

  val zero: Probability = apply(0L, 1L)

  val one: Probability = apply(1L, 1L)

  /**
    * Normalizes a sequence of values, maintaining sequence order.
    */
  def normalizeValues[N](seq: Iterable[N])(implicit num: FrequencyNumeric[N]): Iterable[Probability] = {
    require(seq.nonEmpty, s"Cannot normalize; Normalization on an empty set of pairs is undefined, on $seq")
    val sumN: N = seq.foldLeft(num.zero)(num.plus)
    require(num.gt(sumN, num.zero), s"Cannot normalize pairs, got a zero sum $sumN with implicit numeric $num on $seq")
    seq.map(n => num.toProbability(n, sumN))
  }

  /**
    * Normalizes a sequence of values, mapped by their domain.
    *
    * This guarantees accumulation on the domain, but returns a Map with no guaranteed ordering.
    */
  def normalizePairs[A, N](seq: Iterable[(A, N)])(implicit num: FrequencyNumeric[N]): Map[A, Probability] = {
    require(seq.nonEmpty, s"Cannot normalize; Normalization on an empty set of pairs is undefined, on $seq")

    val sumN: N = seq.foldLeft(num.zero)((s, pair) => num.plus(s, pair._2))
    require(num.gt(sumN, num.zero), s"Cannot normalize pairs, got a zero sum $sumN with implicit numeric $num on $seq")

    seq.foldLeft(Map.empty[A, Probability]) { (memo, pair) =>
      val oldProb = memo.getOrElse(pair._1, Probability.zero)
      val newProb = oldProb + num.toProbability(pair._2, sumN)
      memo.updated(pair._1, newProb)
    }
  }

  trait ProbabilityOrdering extends Ordering[Probability] {
    override def compare(x: Probability, y: Probability): Int = x compare y
  }
  object ProbabilityOrdering extends ProbabilityOrdering

  trait ProbabilityIsConflicted extends Numeric[Probability] with ProbabilityOrdering {
    override def plus(x: Probability, y: Probability): Probability = x + y
    override def minus(x: Probability, y: Probability): Probability = x - y
    override def times(x: Probability, y: Probability): Probability = x * y
    override def negate(x: Probability): Probability = throw new ArithmeticException(s"$x cannot be negated because Probability must be positive")
    override def fromInt(x: Int): Probability = if (x <= 0) zero else one
    override def toInt(x: Probability): Int = x.toDouble.toInt
    override def toLong(x: Probability): Long = x.toDouble.toLong
    override def toFloat(x: Probability): Float = x.toDouble.toFloat
    override def toDouble(x: Probability): Double = x.toDouble
  }
  object ProbabilityIsConflicted extends ProbabilityIsConflicted

  trait ProbabilityIsFractional extends Fractional[Probability] with ProbabilityIsConflicted {
    override def zero: Probability = Probability.zero
    override def one: Probability = Probability.one
    override def div(x: Probability, y: Probability): Probability = x / y
  }
  implicit object ProbabilityIsFractional extends ProbabilityIsFractional

}