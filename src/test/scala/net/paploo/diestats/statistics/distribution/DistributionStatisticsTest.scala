package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.util.{Probability, RandomGenerator}
import net.paploo.diestats.test.{SpecTest, TestBinomialDistributionFourTrialsOfOneOverFour}

class DistributionStatisticsTest extends SpecTest {

  describe("Numeric Distribution Statistics") {

    val distProps = new TestBinomialDistributionFourTrialsOfOneOverFour {}

    val stats = DistributionStatistics.fromNumericDistributionPairs(
      distProps.pairs.reverse //Put in the wrong order to make sure sorting works.
    )

    describe("Basic Properties") {

      it("should return the sorted pairs") {
        stats.pairs should ===(distProps.pairs)
      }

      it("should calculate the cumulative pairs") {
        stats.cumulativePairs should ===(distProps.cumulativePairs)
      }

      it("should calculate the domain") {
        stats.domain should ===(distProps.domain)
      }

      it("should calculate the domain min") {
        stats.min should ===(0L)
      }

      it("should calculate the domain max") {
        stats.max should ===(distProps.trials)
      }

      it("should calculate the frequency sum") {
        stats.sum should ===(Probability.one)
      }

      it("should calculate the modes") {
        stats.modes should ===(distProps.modes)
      }

      it("should calculate the median") {
        stats.median should ===(distProps.median)
      }

      describe("percentile") {

        it("should return the lowest value at the 0th percentile") {
          stats.percentile(Probability.zero) should === (stats.min)
        }

        it("should return the highest value at the 100th percentile") {
          stats.percentile(Probability.one) should === (stats.max)
        }

        it("should calculate the percentile inclusive to the right-end of a domain value") {
          stats.percentile(Probability(242, 256)) should === (2L)
          stats.percentile(Probability(243, 256)) should === (2L)
          stats.percentile(Probability(244, 256)) should === (3L)
        }

      }

    }

    describe("Numeric Properties") {

      it("should calculate the mean") {
        stats.mean should === (distProps.mean +- ε)
      }

      it("should calculate the variance") {
        stats.variance should === (distProps.variance +- ε)
      }

      it("should calculate the standard deviation") {
        stats.stdDev should === (distProps.stdDev +- ε)
      }

      it("should calculate the skewness") {
        stats.skewness should === (distProps.skewness +- ε)
      }

      it("should calculate the kurtosis") {
        stats.kurtosis should === (distProps.kurtosis +- ε)
      }

    }

  }

  describe("More percentile tests") {

    val stats = DistributionStatistics.fromNumericDistributionPairs(
      // From https://en.wikipedia.org/wiki/Percentile
      Seq(
        15L -> 1L,
        20L -> 1L,
        35L -> 1L,
        40L -> 1L,
        50L -> 1L
      )
    )

    it("should have the right cumulativePairs") {
      stats.cumulativePairs should === (Seq(
        15L -> 1,
        20L -> 2,
        35L -> 3,
        40L -> 4,
        50L -> 5
      ))
    }

    describe("percentile") {

      it("should return the lowest value at the 0th percentile") {
        stats.percentile(Probability.zero) should ===(15L)
      }

      it("should return the highest value at the 100th percentile") {
        stats.percentile(Probability.one) should ===(50L)
      }

      it("should calculate the percentile inclusive to the right-end of the percentile") {
        stats.percentile(Probability(39, 100)) should ===(20L)
        stats.percentile(Probability(40, 100)) should ===(20L)
        stats.percentile(Probability(41, 100)) should ===(35L)
      }

    }

    describe("percentileLeft") {

      it("should return the lowest value at the 0th percentile") {
        stats.percentileLeft(Probability.zero) should ===(15L)
      }

      it("should return the highest value at the 100th percentile") {
        stats.percentileLeft(Probability.one) should ===(50L)
      }

      it("should calculate the percentile inclusive to the right-end of the percentile") {
        stats.percentileLeft(Probability(39, 100)) should ===(20L)
        stats.percentileLeft(Probability(40, 100)) should ===(35L) //This is the big difference, we aren't right-side inclusive!
        stats.percentileLeft(Probability(41, 100)) should ===(35L)
      }

    }

  }

  describe("random") {

    val stats = DistributionStatistics.fromDistributionPairs(
      // From https://en.wikipedia.org/wiki/Percentile
      Seq(
        "A" -> 1L,
        "B" -> 2L,
        "C" -> 2L,
        "D" -> 0L
      )
    )

    it("should generate the expected values") {

      val values = (0 until 5).map { i =>
        implicit val rand = RandomGenerator(Probability(i, 5))
        stats.selectRandom
      }

      values should === (Seq("A", "B", "B", "C", "C"))

    }

  }

}
