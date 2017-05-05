package net.paploo.diestats.expression.evaluator

/**
  * The base trait for evaluators of expressions.
  *
  * Defines functionality that is expected of every domain.
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait Evaluator[A, R] {
  def fromValues(as: Iterable[A]): R

  def convolve(x: R, y: R): R
  def repeatedConvolve(n: Int, x: R): R = {
    require(n > 0, s"repeatedConvolution must hapeen more than zero times, but got $n, when convolving over $x")
    Seq.fill(n)(x).reduce(convolve)
  }
}

object Evaluator {

  def direct[A](random: java.util.Random)(implicit numeric: Integral[A]): DirectEvaluator[A] =
    DirectEvaluator.apply(random)

}
