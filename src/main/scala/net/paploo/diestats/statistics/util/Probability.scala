package net.paploo.diestats.statistics.util

import scala.language.implicitConversions

/**
  * Wrapper class for Probabilities.
  *
  * This would be an AnyVal value class, however its special range constraints
  * no longer make it a straight replacement anyway.
  *
  * Probability seeks to guarantee freedom from rounding errors during computations
  * (which is especially important with the [0,1] bounds), and is guaranteed precise
  * when constructed with a rational number; however construction from inexact decimal
  * representations (e.g. Double) can lead to losses in precision.
  *
  * The implementation of this guarantee can change. In the current iteration, we represent
  * probabilities with rational values, however this could change to a BigDecimal in the future.
  *
  * There is no guarantee that the rational value will be properly reduced (e.g. 6/8 -> 3/4);
  * however most operations attempt to do so.
  */
case class Probability(numerator: BigInt, denominator: BigInt) extends Ordered[Probability] {
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

  private[Probability] def reduce: Probability =  {
    val gcd = numerator gcd denominator
    new Probability(numerator/gcd, denominator/gcd) //Use `new` since companion apply calls reduce!
  }

  def toDouble: Double = numerator.toDouble / denominator.toDouble

  def toBigDecimal: BigDecimal = BigDecimal(numerator) / BigDecimal(denominator)
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

  def apply(numerator: BigInt, denominator: BigInt): Probability = new Probability(numerator, denominator).reduce

  val zero: Probability = apply(0L, 1L)

  val one: Probability = apply(1L, 1L)

  def normalizeValues[N](seq: Iterable[N])(implicit num: Numeric[N]): Seq[Probability] = {
    val sum: N = seq.foldLeft(num.zero)(num.plus)
    seq.map(n => Probability(num.toLong(n), num.toLong(sum))).toSeq
  }

  def normalizePairs[A, N](seq: Iterable[(A, N)])(implicit num: Numeric[N]): Seq[(A, Probability)] = {
    val sum: N = seq.foldLeft(num.zero)((s, pair) => num.plus(s, pair._2))
    seq.map(pair => pair._1 -> Probability(num.toLong(pair._2), num.toLong(sum))).toSeq
  }

  val fractionalTypeclass: Fractional[Probability] = new Fractional[Probability] {

    override def zero: Probability = Probability.zero

    override def one: Probability = Probability.one

    override def div(x: Probability, y: Probability): Probability = x / y

    override def plus(x: Probability, y: Probability): Probability = x + y

    override def minus(x: Probability, y: Probability): Probability = x - y

    override def times(x: Probability, y: Probability): Probability = x * y

    override def negate(x: Probability): Probability = throw new ArithmeticException(s"Probability cannot contain negative value $x")

    override def fromInt(x: Int): Probability = if (x <= 0) zero else one

    override def toInt(x: Probability): Int = x.toDouble.toInt

    override def toLong(x: Probability): Long = x.toDouble.toLong

    override def toFloat(x: Probability): Float = x.toDouble.toFloat

    override def toDouble(x: Probability): Double = x.toDouble

    override def compare(x: Probability, y: Probability): Int = x compare y
  }

  trait Implicits {
    implicit def probabilityToDouble(prob: Probability): Double = prob.toDouble

    implicit val fractionalTypeclass: Fractional[Probability] = Probability.fractionalTypeclass
  }
  object Implicits extends Implicits

}
