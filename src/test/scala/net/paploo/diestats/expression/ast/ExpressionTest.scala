package net.paploo.diestats.expression.ast

import net.paploo.diestats.expression.evaluator.{DiceExpressionStringEvaluator, DirectEvaluator, NumericEvaluator, ProbabilityDistributionEvaluator, StringMemoryEvaluator}
import net.paploo.diestats.statistics.util.{AdditionalOrderings, CommutativeMonoid, Monoid, Probability}
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

    // By using a commutative monoid the order in which Tails/Heads is seen during convolve doesn't affect the outcome,
    // because concat(Seq(Tails), Seq(Heads)) will produce the same result as concat(Seq(Heads), Seq(Tails); in this case, the number of heads and tails.
    val tossesCommutativeMonoid: Monoid[Tosses] = CommutativeMonoid.SeqMonoid[Toss]

    // By using the non-commutative monoid, convolution preserves ordering.
    implicit val tossesMonoid: Monoid[Tosses] = Monoid.SeqMonoid[Toss]

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
        val result = coinExpr.apply(ProbabilityDistributionEvaluator.ordered)
        result.pairs should === (Seq(
          Seq(Tails) -> Probability(1,2),
          Seq(Heads) -> Probability(1,2)
        ))
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
        
        val result = convolveExpr.apply(ProbabilityDistributionEvaluator.ordered(tossesMonoid, tossesOrdering))
        result.pairs should === (Seq(
          Seq(Tails, Tails) -> Probability(1,4),
          Seq(Tails, Heads) -> Probability(1,4),
          Seq(Heads, Tails) -> Probability(1,4),
          Seq(Heads, Heads) -> Probability(1,4)
        ))
      }

    }

    describe("best") {

      val coinExpr = Expression.Values(Tosses(Tails), Tosses(Heads))
      val bestExpr = Expression.Best(3, Seq(coinExpr, coinExpr, coinExpr, coinExpr))

      it("should evaluate to string") {
        val result = bestExpr.apply(stringEvaluator())
        result should === ("(List({List(Tails), List(Heads)}, {List(Tails), List(Heads)}, {List(Tails), List(Heads)}, {List(Tails), List(Heads)}) b 3)")
      }

      it("should evaluate to a value") {
        val resultL = bestExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails, Tails, Tails))

        val resultR = bestExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads, Heads, Heads))
      }

      it("should have the right probability distribution domain") {
        val result = bestExpr.apply(ProbabilityDistributionEvaluator.ordered(tossesCommutativeMonoid, tossesOrdering))
        result.pairs should === (Seq(
          //Step 1: Build a list of all the combinations:
          //List(Tails, Tails, Tails, Tails) -> Probability(1,16),
          //List(Tails, Tails, Tails, Heads) -> Probability(1,16),
          //List(Tails, Tails, Heads, Tails) -> Probability(1,16),
          //List(Tails, Tails, Heads, Heads) -> Probability(1,16),
          //List(Tails, Heads, Tails, Tails) -> Probability(1,16),
          //List(Tails, Heads, Tails, Heads) -> Probability(1,16),
          //List(Tails, Heads, Heads, Tails) -> Probability(1,16),
          //List(Tails, Heads, Heads, Heads) -> Probability(1,16),
          //List(Heads, Tails, Tails, Tails) -> Probability(1,16),
          //List(Heads, Tails, Tails, Heads) -> Probability(1,16),
          //List(Heads, Tails, Heads, Tails) -> Probability(1,16),
          //List(Heads, Tails, Heads, Heads) -> Probability(1,16),
          //List(Heads, Heads, Tails, Tails) -> Probability(1,16),
          //List(Heads, Heads, Tails, Heads) -> Probability(1,16),
          //List(Heads, Heads, Heads, Tails) -> Probability(1,16),
          //List(Heads, Heads, Heads, Heads) -> Probability(1,16),

          //Step 2: Extract only the best three:
          //List(Tails, Tails, Tails) -> Probability(1,16),
          //List(Tails, Tails, Heads) -> Probability(1,16),
          //List(Tails, Tails, Heads) -> Probability(1,16),
          //List(Tails, Heads, Heads) -> Probability(1,16),
          //List(Tails, Heads, Tails) -> Probability(1,16),
          //List(Tails, Heads, Heads) -> Probability(1,16),
          //List(Tails, Heads, Heads) -> Probability(1,16),
          //List(Heads, Heads, Heads) -> Probability(1,16),
          //List(Heads, Tails, Tails) -> Probability(1,16),
          //List(Heads, Tails, Heads) -> Probability(1,16),
          //List(Heads, Tails, Heads) -> Probability(1,16),
          //List(Heads, Heads, Heads) -> Probability(1,16),
          //List(Heads, Heads, Tails) -> Probability(1,16),
          //List(Heads, Heads, Heads) -> Probability(1,16),
          //List(Heads, Heads, Heads) -> Probability(1,16),
          //List(Heads, Heads, Heads) -> Probability(1,16)

          //Step 3: Reduce identical lists, and sort by count of tails/heads, and reduce by count.
          List(Tails, Tails, Tails) -> Probability(1,16),
          List(Tails, Tails, Heads) -> Probability(4,16),
          List(Tails, Heads, Heads) -> Probability(6,16),
          List(Heads, Heads, Heads) -> Probability(5,16)
        ))
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
        result should === ("([coin] + {List(Tails), List(Heads)})")
      }

      it("should evaluate to a value") {
        val resultL = stmtExpr.apply(directEvaluatorFirst())
        resultL should === (List(Tails, Tails))

        val resultR = stmtExpr.apply(directEvaluatorLast())
        resultR should === (List(Heads, Heads))
      }

      it("should have the right probability distribution domain") {
        val result = stmtExpr.apply(ProbabilityDistributionEvaluator.ordered)
        result.pairs should === (Seq(
          Seq(Tails, Tails) -> Probability(1,4),
          Seq(Tails, Heads) -> Probability(1,4),
          Seq(Heads, Tails) -> Probability(1,4),
          Seq(Heads, Heads) -> Probability(1,4)
        ))
      }

    }

  }

  describe("Int Domain Expression") {

    val random = new java.util.Random {
      // For testing, always return the expectation value.
      override def nextInt(bound: Int): Int = (bound - 1) / 2
    }

    def stringEvaluator() = DiceExpressionStringEvaluator[Int]

    def orderedDirectEvaluator() = DirectEvaluator.ordered[Int](random)

    def numericDirectEvaluator() = DirectEvaluator.numeric[Int](random)

    describe("values") {

      val d6 = Expression.Values(1,2,3,4,5,6)

      it("should evaluate to string") {
        val result = d6.apply(stringEvaluator())
        result should === ("{1, 2, 3, 4, 5, 6}")
      }

      it("should evaluate to a value") {
        val resultL = d6.apply(orderedDirectEvaluator())
        resultL should === (3)

        val resultR = d6.apply(numericDirectEvaluator())
        resultR should === (3)
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("die value") {

      val d6 = Expression.DieValue(6)

      it("should evaluate to string") {
        val result = d6.apply(stringEvaluator())
        result should === ("d6")
      }

      it("should not compile with an ordered evaluator (needs numeric)") {
        assertDoesNotCompile("d6.apply(orderedDirectEvaluator())")
      }

      it("should evaluate to a value") {
        val resultR = d6.apply(numericDirectEvaluator())
        resultR should === (3)
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("convolve") {
      val d6 = Expression.DieValue(6)
      val convolveExpr = Expression.Convolve(d6, d6)

      it("should evaluate to string") {
        val result = convolveExpr.apply(stringEvaluator())
        result should === ("(d6 + d6)")
      }

      it("should not compile with an ordered evaluator (needs numeric)") {
        assertDoesNotCompile("convolveExpr.apply(orderedDirectEvaluator())")
      }

      it("should evaluate to a value") {
        val resultR = convolveExpr.apply(numericDirectEvaluator())
        resultR should === (6) // 3 + 3, since evaluates each independently.
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("best") {

      val d6 = Expression.DieValue(6)
      val bestExpr = Expression.Best(3, Seq(d6, d6, d6, d6))

      it("should evaluate to string") {
        val result = bestExpr.apply(stringEvaluator())
        result should === ("(List(d6, d6, d6, d6) b 3)")
      }

      it("should not compile with an ordered evaluator (needs numeric)") {
        assertDoesNotCompile("bestExpr.apply(orderedDirectEvaluator())")
      }

      it("should evaluate to a value") {
        val resultR = bestExpr.apply(numericDirectEvaluator())
        resultR should === (9) //It selects 3, 3, 3 and sums.
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

    describe("statements and memory") {

      implicit val witness = DomainType[Int]

      // The type-inference engine broke-down with deducing the type of the evaluator for the `Statements` case class, and
      // needed hinting; but since it uses higher-kinded types, we needed a type-alias to extract the type parameters.
      type NumericWithStringMemory[A, R] = NumericEvaluator[A, R] with StringMemoryEvaluator[A, R]

      //val d6 = Expression.Values(1, 2, 3, 4, 5, 6)
      val d6 = Expression.DieValue(6)
      val stmtExpr = Expression.Statements[Int, NumericWithStringMemory] (
        Expression.Store("d6", d6), //Note: IntelliJ highlights this as red, even though it compiles.
        Expression.Convolve(Expression.Fetch("d6"), d6)
      )

      it("should evaluate to string") {
        val result = stmtExpr.apply(stringEvaluator())
        result should === ("([d6] + d6)")
      }

      it("should not compile with an ordered evaluator (needs numeric)") {
        assertDoesNotCompile("stmtExpr.apply(orderedDirectEvaluator())")
      }

      it("should evaluate to a value") {
        val resultR = stmtExpr.apply(numericDirectEvaluator())
        resultR should === (6) // 3 + 3
      }

      it("should have the right probability distribution domain") {
        pending
      }

    }

  }

}
