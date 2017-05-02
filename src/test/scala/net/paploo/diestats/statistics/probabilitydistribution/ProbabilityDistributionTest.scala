package net.paploo.diestats.statistics.probabilitydistribution

import net.paploo.diestats.statistics.util.{Monoid, Probability, Ordering => StatOrdering}
import net.paploo.diestats.test.SpecTest

import scala.util.Try

class ProbabilityDistributionTest extends SpecTest {

  describe("construction") {

    implicit val probNumeric = Probability.ProbabilityIsFractional

    it("should error trying to create an empty distribution") {
      val result = Try { ProbabilityDistribution.empty[String] }
      result shouldBe 'failure
      result.failed.get shouldBe a[IllegalArgumentException]
    }

    it("should error trying to create a distribution that sums to zero") {
      val result = Try { ProbabilityDistribution.normalize("a" -> 0L, "b" -> 0L) }
      result shouldBe 'failure
      result.failed.get shouldBe a[IllegalArgumentException]
    }

    it("should construct from Long pairs and normalize") {
      val dist = ProbabilityDistribution.normalize("a" -> 10L, "b" -> 90L)
      dist.size should === (2)
      dist.pairs.map(_._2).sum should === (Probability.one)
      dist.get("a") should === (Some(Probability(1, 10)))
      dist.get("b") should === (Some(Probability(9, 10)))
    }

    it("should construct from Probability pairs and normalize") {
      val dist = ProbabilityDistribution.normalize("a" -> Probability(1, 4), "b" -> Probability(1, 3))
      dist.size should === (2)
      dist.pairs.map(_._2).sum should === (Probability.one)
      dist.get("a") should === (Some(Probability(3, 7)))
      dist.get("b") should === (Some(Probability(4, 7)))
    }

    it("should construct using apply using normalized probabilities") {
      val dist = ProbabilityDistribution("a" -> Probability(1, 10), "b" -> Probability(9, 10))
      dist.size should === (2)
      dist.pairs.map(_._2).sum should === (Probability.one)
      dist.get("a") should === (Some(Probability(1, 10)))
      dist.get("b") should === (Some(Probability(9, 10)))
    }

    it("should construct using apply and error if not normalized") {
      val result = Try { ProbabilityDistribution("a" -> Probability(9, 10), "b" -> Probability(9, 10)) }
      result shouldBe 'failure
      result.failed.get shouldBe a[IllegalArgumentException]
    }

  }

  describe("convolve") {

    describe("on Int Monoid") {

      val d3 = ProbabilityDistribution(
        1 -> Probability(1,3),
        2 -> Probability(1,3),
        3 -> Probability(1,3)
      )

      it("should convolve two dice") {
        val twod3 = d3 convolve d3
        twod3.pairs should === (Seq(
          2 -> Probability(1, 9),
          3 -> Probability(2, 9),
          4 -> Probability(3, 9),
          5 -> Probability(2, 9),
          6 -> Probability(1, 9)
        ))
      }

    }

    describe("on String Monoid") {

      val pd = ProbabilityDistribution(
        "T" -> Probability(1, 2),
        "H" -> Probability(1, 2)
      )

      it("should convolved three coin flips") {
        val flipsPD = pd convolve pd convolve pd
        flipsPD.pairs should === (Seq(
          "HHH" -> Probability(1,8),
          "HHT" -> Probability(1,8),
          "HTH" -> Probability(1,8),
          "HTT" -> Probability(1,8),
          "THH" -> Probability(1,8),
          "THT" -> Probability(1,8),
          "TTH" -> Probability(1,8),
          "TTT" -> Probability(1,8)
        ))
      }

    }

    describe("on SeqSet Monoid") {

      val data1 = ProbabilityDistribution(
        Seq("A", "B") -> Probability(1, 2),
        Seq("B", "A") -> Probability(1, 2)
      )

      val data2 = ProbabilityDistribution(
        Seq("A") -> Probability(1, 2),
        Seq("B") -> Probability(1, 2)
      )

      implicit val ordering: Ordering[Seq[String]] = StatOrdering.SeqOrdering

      it("should convolve an ordered seq") {
        implicit val monoid: Monoid[Seq[String]] = Monoid.SeqMonoid

        val result = data1 convolve data2
        result.pairs should ===(Seq(
          Seq("A", "B", "A") -> Probability(1,4),
          Seq("A", "B", "B") -> Probability(1,4),
          Seq("B", "A", "A") -> Probability(1,4),
          Seq("B", "A", "B") -> Probability(1,4)
        ))
      }

      it("should convolve with a non-ordered seq") {
        implicit val monoid: Monoid[Seq[String]] = Monoid.SortedSeqMonoid

        val result = data1 convolve data2
        result.pairs should ===(Seq(
          Seq("A", "A", "B") -> Probability(1,2),
          Seq("A", "B", "B") -> Probability(1,2)
        ))
      }

    }

  }

  describe("mapDomain") {

    val pd = ProbabilityDistribution(
      "TT" -> Probability(1, 4),
      "TH" -> Probability(1, 4),
      "HT" -> Probability(1, 4),
      "HH" -> Probability(1, 4)
    )

    it("should map to a new domain") {
      val result = pd.mapDomain(s => if (s.contains('H')) 'H' else 'T')
      result.pairs should === (Seq(
        'H' -> Probability(3, 4),
        'T' -> Probability(1, 4)
      ))
    }

    it("should aggregate via sum on duplicate pairs") {
      val result = pd.mapDomain(_ => "x")
      result.pairs should === (Seq(
        "x" -> Probability.one
      ))
    }

  }

  describe("mapConvolve") {

    val d3 = ProbabilityDistribution(
      1 -> Probability(1,3),
      2 -> Probability(1,3),
      3 -> Probability(1,3)
    )

    it("should mapConvolve 3d3b2 distribution from d3 repeated 3 times") {
      val result = ProbabilityDistribution.mapConvolve(Seq(d3, d3, d3)) { rolls =>
        rolls.sorted.takeRight(2).sum
      }

      result should === (ProbabilityDistribution(
        2 -> Probability(1, 27),
        3 -> Probability(3, 27),
        4 -> Probability(7, 27),
        5 -> Probability(9, 27),
        6 -> Probability(7, 27)
      ))
    }

    it("should satisfy identity a.convolve(b)(m.reduce) == mapConvolve(Seq(a,b))(m)") {
      val a = d3
      val b = d3
      val m = implicitly[Monoid[Int]]
      ProbabilityDistribution.mapConvolve(Seq(a, b))(m.reduce) should === (a.convolve(b)(m))
    }

  }

}
