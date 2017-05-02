package net.paploo.diestats.expression.ast

import scala.language.higherKinds

trait Evaluator[A, R] {
  def fromValues(as: Seq[A]): R
  def convolve[E[_,_] <: Evaluator[A,R]](x: Expression[A, E], y: Expression[A, E]): R
}

trait NumericEvaluator[A, R] extends Evaluator[A,R] {
  def plus[E[_,_] <: Evaluator[A,R]](x: Expression[A, E], y: Expression[A, E]): R
}

object Evaluator {

  object StringEvaluator extends Evaluator[String, String] {
    override def fromValues(as: Seq[String]): String = as.mkString("")

    override def convolve[E[_, _] <: Evaluator[String, String]](x: Expression[String, E], y: Expression[String, E]): String = {
      val xx = x(this)
      val yy = y(this)
      xx + yy
    }
  }

}

trait Expression[A, -E[_,_]] {
  def apply[R](e: E[A,R]): R
}

object Expression {

  case class Values[A, -E[X,Y] <: Evaluator[X,Y]](values: A*) extends Expression[A, E] {
      override def apply[R](e: E[A, R]): R = e.fromValues(values)
  }

  case class Convolve[A, -E[X,Y] <: Evaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.convolve(x, y)
  }

  case class Plus[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.plus(x, y)
  }


  val foo: Expression[String, Evaluator] = Convolve(Values("foo", "bar"), Values("alpha", "beta"))

  val bar: Expression[Int, NumericEvaluator] = Plus(Values(1,2), Values(3,4))

  //Shouldn't compile because plus requires NumericDomainEvaluator.
  //val bar2: Expression[Int, Evaluator] = Plus(Values(1,2), Values(3,4))


  val cfoo: Expression[String, Evaluator] = Convolve(foo, foo)

  //We don't want this to work, since bar requires NumericDomainEvaluator.
  //val cbar: Expression[Int, Evaluator] = Convolve(bar, bar)

  val cbar2: Expression[Int, NumericEvaluator] = Convolve(bar, bar)

}


