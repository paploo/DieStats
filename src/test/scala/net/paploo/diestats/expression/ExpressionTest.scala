package net.paploo.diestats.expression

import net.paploo.diestats.test.SpecTest

class ExpressionTest extends SpecTest {

  describe("basic example") {

    //TODO: Make real tests!

    it("should produce a values in the requested range") {

      val expr = Expression.applyValues(1,2,3) + Expression.applyValues(1,2,3)

      val evaluator = new ExpressionEvaluator.IntEvaluators.RNGEvaluator()

      val result = expr(evaluator)
      println(result)

      result shouldBe >=(2)
      result shouldBe <=(6)

    }

  }

}
