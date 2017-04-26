package net.paploo.diestats.statistics.util

import net.paploo.diestats.test.SpecTest

import scala.util.Try

class ProbabilityTest extends SpecTest {

  describe("construction") {

    describe("fractional creation") {

      it("should create from a numerator and denominator") {
        val prob = Probability(1, 3)
        prob.toBigDecimal should === (BigDecimal(1) / BigDecimal(3))
      }

      it("should auto simplify the fraction") {
        val prob = Probability(6, 18)
        prob.toBigDecimal should === (BigDecimal(1) / BigDecimal(3))
      }

      it("should give an error if out of bounds") {
        Try(Probability(-1, 3)) shouldBe 'failure // x<0
        Try(Probability(2, 1)) shouldBe 'failure // x>1
        Try(Probability(1, 0)) shouldBe 'failure // x is undefined
      }

      it("should allow one and zero") {
        Probability.zero.toBigDecimal should === (BigDecimal(0))
        Probability.one.toBigDecimal should === (BigDecimal(1))
      }

    }

  }

  describe("comparison") {

    describe("GT") {
      (Probability(2, 3) compare Probability(50, 100)) shouldBe > (0)
    }

    describe("LT") {
      (Probability(50, 100) compare Probability(2, 3)) shouldBe < (0)
    }

    describe("EQ") {
      (Probability(50, 100) compare Probability(1, 2)) shouldBe (0)
    }

  }

  describe("arithmetic") {

    describe("addition") {

      it("should add") {
        val prob = Probability(3, 4) + Probability(1, 8)
        prob should === (Probability(7, 8))
      }

      it("should reduce the result") {
        val prob = Probability(3, 4) + Probability(1, 8)
        prob.numerator should === (7)
        prob.denominator should === (8)
      }

      it("should error if addition is out of bounds") {
        Try(Probability(3,4) + Probability(7,8)) shouldBe 'failure
      }

    }

    describe("multiplication") {

      it("should multiply") {
        val prob = Probability(3, 4) * Probability(2, 9)
        prob should === (Probability(1, 6))
      }

      it("should reduce the result") {
        val prob = Probability(3, 4) * Probability(2, 9)
        prob.numerator should ===(1)
        prob.denominator should ===(6)
      }

    }

  }

  describe("Implicits") {

    val x = Probability(1, 4)
    val y = Probability(3, 4)

    it("should resolve the ordering") {
      val ord = implicitly[Ordering[Probability]]
      ord.compare(x, y) should === (-1)
    }

    it("should resolve the numeric") {
      val num = implicitly[Numeric[Probability]]
      num.times(x, y) should === (Probability(3, 16))
    }

    it("hould resolve the fractional") {
      val frac = implicitly[Fractional[Probability]]
      frac.div(x, y) should === (Probability(1, 3))
    }

    it("should resolve the frequencyNumeric") {
      //TODO: Move this test to FrequencyNumeric?
      val fn = implicitly[FrequencyNumeric[Probability]]
      fn.toProbability(x, y) should === (Probability(1, 3))
    }

  }

}
