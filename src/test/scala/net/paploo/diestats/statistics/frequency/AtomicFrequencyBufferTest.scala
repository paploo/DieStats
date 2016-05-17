package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.test.SpecTest

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class AtomicFrequencyBufferTest extends SpecTest {

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
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      freq.domain should === (SampleDomain.values)
    }

  }

  describe("increment and add") {

    it("should increment the domain value count by one") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      freq.increment(SampleDomain.Alpha)
      freq(SampleDomain.Alpha) should === (1)
    }

    it("should increment the domain value count by a delta") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      freq.add(SampleDomain.Alpha, 10L)
      freq(SampleDomain.Alpha) should === (10L)
    }

    it("should ignore counts from unknown domain values by default") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      freq.increment(SampleDomain.Unknown)
      freq(SampleDomain.Alpha) should === (0)
    }

    it("should use the unknown value handler to recast to a different doamin value") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values, (_: SampleDomain) => Some(SampleDomain.Alpha))
      freq.increment(SampleDomain.Unknown)
      freq(SampleDomain.Alpha) should === (1)
    }

    it("should return the frequency object") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      val result = freq.add(SampleDomain.Alpha, 10L)
      result should be (freq)
    }

  }

  describe("counts") {

    it("should return the total counts added") {
      val freq = AtomicFrequencyBuffer(SampleDomain.values)
      for {
        domain <- SampleDomain.values
        i <- 0 until 100
      } freq.add(domain, 3)

      freq.counts should === (SampleDomain.values.length.toLong * 100L * 3L)
    }

    // Not a perfect test, but on the non-atomic implementation it fails
    // every time I run it, so it can't be too bad.
    it("is be thread safe") {
      import ExecutionContext.Implicits.global

      val domain = SampleDomain.values.head
      val processors = Runtime.getRuntime.availableProcessors
      val n = 10000
      val freq = AtomicFrequencyBuffer(SampleDomain.values)

      val futures = (0 until processors).map { i =>
        Future((0 until n).foreach { j => freq.increment(domain) })
      }
      Await.ready(Future.sequence(futures), Duration.Inf)

      freq.counts should === (processors * n)
    }

  }

  describe("pdf") {

    //TODO: Implement.

  }

}
