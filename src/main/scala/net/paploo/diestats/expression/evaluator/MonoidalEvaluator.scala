package net.paploo.diestats.expression.evaluator

import net.paploo.diestats.statistics.util.Monoid

/**
  * The base trait for evaluators over a monoidal output type.
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait MonoidalEvaluator[A, R] extends Evaluator[A, R] {
  def monoid: Monoid[R]

  override def convolve(x: R, y: R): R = monoid.concat(x, y)
}
