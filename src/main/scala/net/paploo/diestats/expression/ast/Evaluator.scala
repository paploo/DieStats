//package net.paploo.diestats.expression.ast
//
//import net.paploo.diestats.statistics.util.Monoid
//
///**
//  * The base trait for evaluators.
//  *
//  * @tparam A The domain type
//  * @tparam R The evaluation result type
//  */
//trait Evaluator[A, R] {
//  def fromValues(as: Iterable[A]): R
//}
//
///**
//  * The base trait for evaluators over a monoidal domain.
//  * @tparam A The domain type
//  * @tparam R The evaluation result type
//  */
//trait MonoidalEvaluator[A, R] extends Evaluator[A, R] {
//  def monoid: Monoid[A]
//
//  def convolve(x: Expression[A, Evaluator], y: Expression[A, Evaluator]): R
//  def repeatedConvolve(n: Int, x: Expression[A, Evaluator]): R
//}
//
///**
//  * The base trait for evaluators over an ordered, monoidal domain.
//  * @tparam A The domain type
//  * @tparam R The evaluation result type
//  */
//trait OrderedEvaluator[A, R] extends MonoidalEvaluator[A, R] {
//  def ordering: Ordering[A]
//
//  def best(n: Int, xs: Seq[Expression[A]]): R
//  def worst(n: Int, xs: Seq[Expression[A]]): R
//}
//
///**
//  * The base trait for evaluators over a numeric, ordered, and monoidal domain.
//  * @tparam A The domain type
//  * @tparam R The evaluation result type
//  */
//trait NumericDomainEvaluator[A, R] extends OrderedEvaluator[A, R] {
//  def numeric: Numeric[A]
//
//  def repeatedConvolve(n: Expression[A], x: Expression[A]): R
//
//  def plus(x: Expression[A], y: Expression[A]): R = convolve(x, y)
//  def minus(x: Expression[A], y: Expression[A]): R
//  def times(x: Expression[A], y: Expression[A]): R
//  def div(x: Expression[A], y: Expression[A]): R
//}
