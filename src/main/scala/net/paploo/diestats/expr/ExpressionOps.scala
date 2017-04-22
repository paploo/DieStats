package net.paploo.diestats.expr

/**
  * Trait expressing an implementation of operations over expressions on domain A,
  * returning value R.
  *
  * Example concrete types for R include a PDF for analysis, or even A in random value generation.
  *
  * ExpressionOps contains a mixture of operations that are expected, but only a
  * subset can be done on some domains. In the current design, it is up to the
  * implementation do decide how to handle non-sensical operations for a given
  * domain. For example, some consumers of this library may decide to throw
  * exceptions, while others may parameterize R over an Either or Try, while
  * others may make specialized validation ExpressionOps implementations for
  * a domain.
  * @tparam A The domain
  * @tparam R The operation aggregation value.
  */
trait ExpressionOps[A, R] {

  def apply(values: Seq[A]): R

  def applyCounts(valueCounts: Seq[(A, Long)]): R

  def plus(x: Expression[A, R], y: Expression[A, R]): R

  def minus(x: Expression[A, R], y: Expression[A, R]): R

  def times(X: Expression[A, R], y: Expression[A, R]): R

  def div(X: Expression[A, R], y: Expression[A, R]): R

  def negate(x: Expression[A, R]): R

  def repeat(times: Int, x: Expression[A, R]): R

  def takeLower(n: Int, exprs: Iterable[Expression[A, R]]): R

  def takeUpper(n: Int, exprs: Iterable[Expression[A, R]]): R

}

object ExpressionOps {

  /**
    * The most common domain—used in almost all board games—is an Int domain.
    *
    * We define both a PDF implementation, and a random value generation implementation.
    */
  object IntDomainExpressionOps {

    class Random {

    }

    class PDF {

    }

  }

}
