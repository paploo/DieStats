package net.paploo.diestats.expression.evaluator

import net.paploo.diestats.statistics.util.Monoid

import scala.collection.immutable.NumericRange

/**
  * The base trait for evaluators over a numeric domain.
  *
  * Defines functionality that is expected of a numeric domain.
  *
  * Note that plus is almost always a synonym for convolution.
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait NumericEvaluator[A, R] extends OrderedEvaluator[A, R] {
  def plus(x: R, y: R): R = convolve(x, y)
  def minus(x: R, y: R): R
  def times(x: R, y: R): R
  def quot(x: R, y: R): R

  def negate(x: R): R

  def rangedValues(max: A): R
  def rangedValues(min: A, max: A): R
}
