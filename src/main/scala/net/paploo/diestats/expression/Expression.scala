package net.paploo.diestats.expression

/**
  * Base trait for expressions over a domain of type A.
  *
  * Expressions define an AST for forming values over a domain A, with evaluation of operations
  * based withheld until an appropriate evaluation context is applied to generate a result.
  *
  * Example contexts include evalutation to ProbabilityDistributions, Statistics, or Randomly Generated Values.
  *
  * @tparam A The expression domain.
  * @tparam R The evaluation result type.
  */
trait Expression[A, R] extends (ExpressionOps[A, R] => R) {

  def +(that: Expression[A, R]): Expression[A, R] = Expression.AST.Plus(this, that)

  def -(that: Expression[A, R]): Expression[A, R] = Expression.AST.Minus(this, that)

  def *(that: Expression[A, R]): Expression[A, R] = Expression.AST.Times(this, that)

  def /(that: Expression[A, R]): Expression[A, R] = Expression.AST.Div(this, that)

  def unary_- : Expression[A, R] = Expression.AST.Negate(this)

  def repeat(times: Int): Expression[A, R] = Expression.AST.Repeat(this, times)

}

object Expression {

  // === Convenience Factory Methods ===

  def applyValues[A, R](values: A*): Expression[A, R] = Expression.AST.Values(values)

  def applyCounts[A, R](valueCounts: (A, Long)*): Expression[A, R] = Expression.AST.ValueCounts(valueCounts)

  def takeLower[A, R](n: Int, exprs: Iterable[Expression[A, R]]): Expression[A, R] = Expression.AST.TakeLower(n, exprs)

  def takeUpper[A, R](n: Int, exprs: Iterable[Expression[A, R]]): Expression[A, R] = Expression.AST.TakeUpper(n, exprs)

  // === Expression Subclasses ===

  object AST {

    case class Plus[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.plus(x, y)
    }

    case class Minus[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.minus(x, y)
    }

    case class Times[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.times(x, y)
    }

    case class Div[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.div(x, y)
    }

    case class Negate[A, R](x: Expression[A, R]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.negate(x)
    }

    case class Repeat[A, R](x: Expression[A, R], times: Int) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.repeat(times, x)
    }

    case class TakeLower[A, R](n: Int, exprs: Iterable[Expression[A, R]]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.takeLower(n, exprs)
    }

    case class TakeUpper[A, R](n: Int, exprs: Iterable[Expression[A, R]]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.takeUpper(n, exprs)
    }

    case class Values[A, R](values: Iterable[A]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.fromValues(values)
    }

    case class ValueCounts[A, R](valueCounts: Iterable[(A, Long)]) extends Expression[A, R] {
      override def apply(ops: ExpressionOps[A, R]): R = ops.fromCounts(valueCounts)
    }

  }

}