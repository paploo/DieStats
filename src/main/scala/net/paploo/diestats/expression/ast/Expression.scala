package net.paploo.diestats.expression.ast

import scala.language.higherKinds

trait Evaluator[A, R]
trait NumericDomainEvaluator[A, R] extends Evaluator[A,R]

trait Expression[A, -E[_,_] <: Evaluator[_,_]] {
  def apply[R](evaluator: E[A, R]): R
}

object Expression {

  case class Convolve[A, EE[_,_] <: Evaluator[_,_]](x: Expression[A, EE], y: Expression[A, EE]) extends Expression[A, EE] {
    override def apply[R](evaluator: EE[A, R]): R = ???
  }

  case class Plus[A, EE[_,_] <: NumericDomainEvaluator[_,_]](x: Expression[A, EE], y: Expression[A, EE]) extends Expression[A, EE] {
    override def apply[R](evaluator: EE[A, R]): R = ???
  }

  case class Values[A](values: A*) extends Expression[A, Evaluator] {
    override def apply[R](evaluator: Evaluator[A, R]): R = ???
  }

  val foo: Expression[String, Evaluator] = Convolve(Values("foo", "bar"), Values("alpha", "beta"))

  val bar: Expression[Int, NumericDomainEvaluator] = Plus(Values(1,2), Values(3,4))

  val cfoo = Convolve(foo, foo)

  //We don't want this to work, since bar requires NumericDomainEvaluator.
  val cbar: Expression[Int, Evaluator] = Convolve(bar, bar)

  val cbar2: Expression[Int, NumericDomainEvaluator] = Convolve(bar, bar)

}


