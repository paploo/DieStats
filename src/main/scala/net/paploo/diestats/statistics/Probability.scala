package net.paploo.diestats.statistics

import scala.language.implicitConversions

/**
  * Value wrapper class for probabilities.
  */
class Probability(val toDouble: Double) extends AnyVal {
  def +(that: Probability): Probability = Probability(this.toDouble + that.toDouble)
  def *(that: Probability): Probability = Probability(this.toDouble * that.toDouble)

  def -(that: Probability): Probability = Probability(this.toDouble - that.toDouble)
  def /(that: Probability): Probability = Probability(this.toDouble / that.toDouble)

  def unary_- = Probability(-this.toDouble)

  def compliment: Probability = Probability(1.0 - this.toDouble)
}

object Probability {

  val zero: Probability = apply(0.0)

  val one: Probability = apply(1.0)

  def apply(p: Double): Probability = {
    // Have to enforce constraints here, because value classes can't do it in the class itself.
    require(p >= 0.0, s"Probabilities must be positive in value for $p.")
    require(p <= 1.0, s"Probabilities must be less than one, but got $p.")
    new Probability(p)
  }

  def unapply(prob: Probability): Option[Double] = Some(prob.toDouble)

  def normalizeValues[N](seq: TraversableOnce[N])(implicit num: Numeric[N]): Seq[Probability] = {
    val sumN: N = seq.foldLeft(num.zero)(num.plus)
    val sum: Double = num.toDouble(sumN)
    seq.map(n => Probability(num.toDouble(n) / sum)).toSeq
  }

  def normalizePairs[A, N](seq: TraversableOnce[(A, N)])(implicit num: Numeric[N]): Seq[(A, Probability)] = {
    val sumN: N = seq.foldLeft(num.zero){ case (s, (a,p)) => num.plus(s, p) }
    val sum: Double = num.toDouble(sumN)
    seq.map(pair => pair._1 -> Probability(num.toDouble(pair._2) / sum)).toSeq
  }

  val fractionalTypeclass: Fractional[Probability] = new Fractional[Probability] {

    override def zero: Probability = Probability.zero

    override def one: Probability = Probability.one

    override def div(x: Probability, y: Probability): Probability = x / y

    override def plus(x: Probability, y: Probability): Probability = x + y

    override def minus(x: Probability, y: Probability): Probability = x - y

    override def times(x: Probability, y: Probability): Probability = x * y

    override def negate(x: Probability): Probability = -x

    override def fromInt(x: Int): Probability = if (x <= 0) zero else one

    override def toInt(x: Probability): Int = x.toDouble.toInt

    override def toLong(x: Probability): Long = x.toDouble.toLong

    override def toFloat(x: Probability): Float = x.toDouble.toFloat

    override def toDouble(x: Probability): Double = x.toDouble

    override def compare(x: Probability, y: Probability): Int = x.toDouble compare y.toDouble
  }

  trait Implicits {
    implicit def probabilityToDouble(prob: Probability): Double = prob.toDouble
    implicit def doubleToProbability(double: Double): Probability = Probability(double)

    implicit val fractionalTypeclass: Fractional[Probability] = Probability.fractionalTypeclass
  }

  object Implicits extends Implicits

}
