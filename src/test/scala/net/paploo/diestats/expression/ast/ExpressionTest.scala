package net.paploo.diestats.expression.ast

import net.paploo.diestats.expression.evaluator.{DiceExpressionStringEvaluator, DirectEvaluator}
import net.paploo.diestats.statistics.util.{AdditionalOrderings, Monoid}
import net.paploo.diestats.test.SpecTest

class ExpressionTest extends SpecTest {

  describe("Coin Toss Expression") {

    sealed trait Toss
    case object Heads extends Toss
    case object Tails extends Toss

    implicit val tossOrdering: Ordering[Toss] = (x: Toss, y: Toss) => (x, y) match {
      case (Tails, Heads) => -1
      case (Heads, Tails) => 1
      case _ => 0
    }

    type Tosses = Seq[Toss]
    object Tosses {
      def apply(tosses: Toss*): Tosses = tosses.toList
    }

    implicit val tossesMonoid: Monoid[Tosses] = new Monoid[Tosses] {
      override def concat(x: Tosses, y: Tosses): Tosses = x ++ y
      override def empty: Tosses = List.empty
    }

    implicit val tossesOrdering: Ordering[Tosses] = AdditionalOrderings.SeqOrdering[Toss]

    def stringEvaluator() = DiceExpressionStringEvaluator[Tosses]

    def directEvaluatorFirst() = DirectEvaluator.ordered(new java.util.Random {
      //This is used for element selection; we want something predictable and easy to reason about.
      override def nextInt(bound: Int): Int = 0
    })

    def directEvaluatorLast() = DirectEvaluator.ordered(new java.util.Random {
      //This is used for element selection; we want something predictable and easy to reason about.
      override def nextInt(bound: Int): Int = bound - 1
    })


    describe("values") {

      val coinExpr = Expression.Values(Tosses(Tails), Tosses(Heads))

      it("should evaluate to string") {
        val result = coinExpr.apply(stringEvaluator())
        result should === ("{List(Tails), List(Heads)}")
      }

      it("should evaluate to a value") {
        val resultL = coinExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails))

        val resultR = coinExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads))
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("convolve") {

      val coinExpr = Expression.Values(Tosses(Tails), Tosses(Heads))
      val convolveExpr = Expression.Convolve(coinExpr, coinExpr)

      it("should evaluate to string") {
        val result = convolveExpr.apply(stringEvaluator())
        result should === ("({List(Tails), List(Heads)} + {List(Tails), List(Heads)})")
      }

      it("should evaluate to a value") {
        val resultL = convolveExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails, Tails))

        val resultR = convolveExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads, Heads))
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("best") {

      val coinExpr = Expression.Values(Tosses(Tails), Tosses(Heads))
      val bestExpr = Expression.Best(3, Seq(coinExpr, coinExpr, coinExpr))

      it("should evaluate to string") {
        val result = bestExpr.apply(stringEvaluator())
        result should === ("(List({List(Tails), List(Heads)}, {List(Tails), List(Heads)}, {List(Tails), List(Heads)}) b 3)")
      }

      it("should evaluate to a value") {
        val resultL = bestExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails, Tails, Tails))

        val resultR = bestExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads, Heads, Heads))
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("statements and memory") {

      implicit val witness = DomainType[Tosses]

      val coinExpr = Expression.Values(Tosses(Tails), Tosses(Heads))
      val stmtExpr = Expression.Statements(
        Expression.Store("coin", coinExpr),
        Expression.Convolve(Expression.Fetch("coin"), coinExpr)
      )

      it("should evaluate to string") {
        val result = stmtExpr.apply(stringEvaluator())
        result should === ("([id] + {List(Tails), List(Heads)})")
      }

      it("should evaluate to a value") {
        val resultL = stmtExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails, Tails))

        val resultR = stmtExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads, Heads))
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

  }

  describe("Int Domain Expression") {

  }

}
