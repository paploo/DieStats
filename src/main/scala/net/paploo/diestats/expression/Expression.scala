package net.paploo.diestats.expression

/**
  * Base trait for expressions over a domain of type A.
  *
  * Expressions define an AST for forming values over a domain A, with evaluation of operations
  * based withheld until an appropriate evaluation context is applied to generate a result.
  *
  * Example contexts include evalutation to ProbabilityDistributions, Statistics, or Randomly Generated Values.
  *
  * @tparam A The expression domain type.
  */
trait Expression[A] {

  /**
    * Evaluates this expression node to a value of type R using the given evaluator.
    *
    * @param evaluator
    * @tparam R
    * @return
    */
  def apply[R](evaluator: ExpressionEvaluator[A, R]): R

  def +(that: Expression[A]): Expression[A] = Expression.AST.Plus(this, that)

  def -(that: Expression[A]): Expression[A] = Expression.AST.Minus(this, that)

  def *(that: Expression[A]): Expression[A] = Expression.AST.Times(this, that)

  def /(that: Expression[A]): Expression[A] = Expression.AST.Div(this, that)

  def unary_- : Expression[A] = Expression.AST.Negate(this)

  def repeat(times: Int): Expression[A] = Expression.AST.Repeat(this, times)

}

object Expression {

  // === Convenience Factory Methods ===

  def applyValues[A](values: A*): Expression[A] = Expression.AST.Values(values)

  def applyCounts[A](valueCounts: (A, Long)*): Expression[A] = Expression.AST.ValueCounts(valueCounts)

  def takeLower[A](n: Int, exprs: Iterable[Expression[A]]): Expression[A] = Expression.AST.TakeLower(n, exprs)

  def takeUpper[A](n: Int, exprs: Iterable[Expression[A]]): Expression[A] = Expression.AST.TakeUpper(n, exprs)

  // === Expression Subclasses ===

  object AST {

    case class Plus[A](x: Expression[A], y: Expression[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.plus(x, y)
    }

    case class Minus[A](x: Expression[A], y: Expression[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.minus(x, y)
    }

    case class Times[A](x: Expression[A], y: Expression[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.times(x, y)
    }

    case class Div[A](x: Expression[A], y: Expression[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.div(x, y)
    }

    case class Negate[A](x: Expression[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.negate(x)
    }

    case class Repeat[A](x: Expression[A], times: Int) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.repeat(times, x)
    }

    case class TakeLower[A](n: Int, exprs: Iterable[Expression[A]]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.takeLower(n, exprs)
    }

    case class TakeUpper[A](n: Int, exprs: Iterable[Expression[A]]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.takeUpper(n, exprs)
    }

    case class Values[A](values: Iterable[A]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.fromValues(values)
    }

    case class ValueCounts[A](valueCounts: Iterable[(A, Long)]) extends Expression[A] {
      override def apply[R](ops: ExpressionEvaluator[A, R]): R = ops.fromCounts(valueCounts)
    }

  }

}