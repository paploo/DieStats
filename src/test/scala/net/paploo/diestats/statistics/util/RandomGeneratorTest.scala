package net.paploo.diestats.statistics.util

import net.paploo.diestats.test.SpecTest

class RandomGeneratorTest extends SpecTest {

  describe("default implementation") {

    val rand = implicitly[RandomGenerator]

    it("should generate legal values without error") {
      (0 until 10000).foreach {
        _ => rand.nextProbability()
      }
    }

  }

}
