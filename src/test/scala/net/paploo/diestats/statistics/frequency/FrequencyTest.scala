package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.test.SpecTest

class FrequencyTest extends SpecTest {

  describe("object methods") {

    it("should construct an empty frequency") {
      val freq: Frequency[String] = Frequency.empty[String]
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct an empty frequency from apply with no args") {
      val freq: Frequency[String] = Frequency[String]()
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct from pairs") {
      val freq: Frequency[String] = Frequency("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should construct from values") {
      val freq: Frequency[String] = Frequency.fromValues("alpha", "beta", "alpha", "gamma")
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("Should build from an iterable of pairs") {
      val freq: Frequency[String] = Frequency.buildFrom(Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should build an iterable of values") {
      val freq: Frequency[String] = Frequency.buildFromValues(Seq("alpha", "beta", "alpha", "gamma"))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should create empty with a domain") {
      val freq: Frequency[String] = Frequency.emptyWithDomain("alpha", "beta", "gamma")
      freq.sum should === (0L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
    }

  }

  describe("implicit iterable conversion") {

    val freq: Frequency[String] = Frequency("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
    val freqSeq = Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)

    it("should implicitly convert to an iterable") {
      val iterable: Iterable[(String, Long)] = freq
      iterable.toSeq.sorted should === (freqSeq.sorted)
    }

  }

}
