package net.paploo.diestats.expression.evaluator

/**
  * The base trait for evaluators over an ordered domain.
  *
  * Defines functionality that is expected of ordered domains.
  *
  * Best/Worst are the trickiest to think about implementing, but
  * the two main use cases are:
  * 1. When R is a ProbabilityDistribution[A], where mapConvolve is used, and
  * 2. When R =:= A, for example, when evaluating with a random generator.
  *
  * Note that only some sub implementations need define an ordering,
  * and hence none is required here by the trait. For example,
  * a stringifier won't need one, while something that performs the actual
  * computation will need it.
  *
  * @tparam A The domain type
  */
trait OrderedEvaluator[A, R] extends Evaluator[A, R] {
  def best(n: Int, xs: Iterable[R]): R
  def worst(n: Int, xs: Iterable[R]): R
}
