package net.paploo.diestats.statistics

/**
  * Value wrapper class for probabilities.
  */
class Probability(val toDouble: Double) extends AnyVal {
  require(toDouble >= 0.0, "Probabilities must be positive in value.")
  require(toDouble <= 1.0, "Probabilities must be less than one.")

  def +(that: Probability): Probability = Probability(this.toDouble + that.toDouble)
  def *(that: Probability): Probability = Probability(this.toDouble * that.toDouble)

  def compliment: Probability = Probability(1.0 - this.toDouble)
}

object Probability {

  val zero: Probability = apply(0.0)

  val one: Probability = apply(1.0)

  def apply(p: Double): Probability = new Probability(p)

  def unapply(prob: Probability): Option[Double] = Some(prob.toDouble)

  def normalize[N](seq: Seq[N])(implicit num: Numeric[N]): Seq[Probability] = {
    val sumN: N = seq.foldLeft(num.zero)(num.plus)
    val sum: Double = num.toDouble(sumN)
    seq.map(n => Probability(num.toDouble(n) / sum))
  }

  def normalize[A, N](map: Map[A, N])(implicit num: Numeric[N]): Map[A, Probability] = {
    val sumN: N = map.values.foldLeft(num.zero)(num.plus)
    val sum: Double = num.toDouble(sumN)
    map.map { case (a, n) => a -> Probability(num.toDouble(n) / sum)}
  }

  trait Implicits {
    def probabilityToDouble(prob: Probability): Double = prob.toDouble
  }

  object Implicits extends Implicits

}
