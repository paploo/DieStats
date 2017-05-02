package net.paploo.diestats.statistics.util

import net.paploo.diestats.test.SpecTest

class MonoidTest extends SpecTest {

  describe("reduce") {

    it("should reduce a string") {
      val monoid = implicitly[Monoid[String]]
      monoid.reduce(Seq("alpha", "beta", "", "gamma")) should === ("alphabetagamma")
    }

    it("should reduce numbers as summation") {
      val monoid = implicitly[Monoid[Int]]
      monoid.reduce(Seq(5, -1, 0, 2)) should === (6)
    }

  }

}
