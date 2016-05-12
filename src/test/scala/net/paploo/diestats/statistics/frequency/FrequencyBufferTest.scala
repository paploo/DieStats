package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.test.SpecTest

class FrequencyBufferTest extends SpecTest {

  trait SampleDomain
  object SampleDomain {
    object Alpha extends SampleDomain
    object Beta extends SampleDomain
    object Gamma extends SampleDomain

    object Unknown extends SampleDomain //A special value for testing unknown domain values.

    val values: Seq[SampleDomain] = Seq(Alpha, Beta, Gamma)
  }

  describe("domain") {

    it("should return the creation domain") {
      val freq = FrequencyBuffer(SampleDomain.values)
      freq.domain should === (SampleDomain.values)
    }

  }

  describe("increment and add") {

    it("should increment the domain value count by one") {
      val freq = FrequencyBuffer(SampleDomain.values)
      freq.increment(SampleDomain.Alpha)
      freq(SampleDomain.Alpha) should === (1)
    }

    it("should increment the domain value count by a delta") {
      val freq = FrequencyBuffer(SampleDomain.values)
      freq.add(SampleDomain.Alpha, 10L)
      freq(SampleDomain.Alpha) should === (10L)
    }

    it("should ignore counts from unknown domain values by default") {
      val freq = FrequencyBuffer(SampleDomain.values)
      freq.increment(SampleDomain.Unknown)
      freq(SampleDomain.Alpha) should === (0)
    }

    it("should use the unknown value handler to recast to a different doamin value") {
      val freq = FrequencyBuffer(SampleDomain.values, (_: SampleDomain) => Some(SampleDomain.Alpha))
      freq.increment(SampleDomain.Unknown)
      freq(SampleDomain.Alpha) should === (1)
    }

    it("should return the frequency object") {
      val freq = FrequencyBuffer(SampleDomain.values)
      val result = freq.add(SampleDomain.Alpha, 10L)
      result should be (freq)
    }

  }

  describe("ingest") {

  }

  describe("counts") {

  }

  describe("pdf") {

  }

}
