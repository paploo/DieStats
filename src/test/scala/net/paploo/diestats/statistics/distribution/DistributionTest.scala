package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.test.SpecTest

class DistributionTest extends SpecTest {

  describe("reducePairs") {

    val pairs = Seq(("a", 1), ("b", 2), ("a", 5))

    it("should reduce equivalent pairs via addition") {
      val pairs = Seq(("a", 1), ("b", 2), ("a", 5))
      Distribution.reducePairs(pairs).toMap should === (
        Map(("a", 6), ("b", 2))
      )
    }

  }

}
