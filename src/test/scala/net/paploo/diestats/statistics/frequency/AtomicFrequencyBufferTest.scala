package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.test.SpecTest

class AtomicFrequencyBufferTest extends SpecTest {

  describe("object methods") {

    it("should construct an empty frequency") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer.empty[String]
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct an empty frequency from apply with no args") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer[String]()
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct from pairs") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should construct from values") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer.fromValues("alpha", "beta", "alpha", "gamma")
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("Should build from an iterable of pairs") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer.buildFrom(Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should build an iterable of values") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer.buildFromValues(Seq("alpha", "beta", "alpha", "gamma"))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should create empty with a domain") {
      val freq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer.emptyWithDomain("alpha", "beta", "gamma")
      freq.sum should === (0L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
    }

  }

  describe("implicit iterable conversion") {

    def freq(): AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
    val freqSeq = Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)

    it("should implicitly convert to an iterable") {
      val iterable: Iterable[(String, Long)] = freq()
      iterable.toSeq.sorted should === (freqSeq.sorted)
    }

  }

  describe("basic properties") {

    def testFreq(): AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)
    val reverseOrdering: Ordering[String] = Ordering.String.reverse

    describe("get") {

      it("should return Some(count) for a counted domain value") {
        testFreq().get("gamma") should === (Some(20))
      }

      it("should return None for a not counted domain value") {
        testFreq().get("zeta") should === (None)
      }

    }

    describe("count") {

      it("should return the total count") {
        testFreq().sum should === (100)
      }

    }

    describe("domain") {

      it("should return the domain using the default sorting") {
        testFreq().domain should === (Seq("alpha", "beta", "delta", "gamma"))
      }

      it("should sort the domain according to the scoped implicit ordering") {
        implicit val ord = reverseOrdering
        testFreq().domain should === (Seq("gamma", "delta", "beta", "alpha"))
      }

    }

    describe("pairs") {

      it("should return the pairs using the default sorting") {
        testFreq().pairs should === (Seq("alpha" -> 10L, "beta" -> 40L, "delta" -> 30L, "gamma" -> 20L))
      }

      it("should sort the pairs according ot the implicit domain ordering") {
        implicit val ord = reverseOrdering
        testFreq().pairs should === (Seq("gamma" -> 20L, "delta" -> 30L, "beta" -> 40L, "alpha" -> 10L))
      }

    }


  }

  describe("accumulation") {

    def testFreq(): AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)

    describe("+") {

      it("should accumulate on a new domain value") {
        val freq = testFreq() + ("epsilon" -> 7L)
        freq.get("epsilon") should === (Some(7L))
        freq.sum should === (107L)
      }

      it("should increment an existing domain value") {
        val freq = testFreq() + ("alpha" -> 14L)
        freq.get("alpha") should === (Some(24L))
        freq.sum should === (114L)
      }

    }

    describe("+:") {

      it("should accumulate one count on a new domain value") {
        val freq = testFreq() :+ "epsilon"
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (101L)
      }

      it("should accumulate one count on an existing domain value") {
        val freq = testFreq() :+ "alpha"
        freq.get("alpha") should === (Some(11L))
        freq.sum should === (101L)
      }

    }

    describe("++") {

      it("should be identity on an empty set") {
        val freq = testFreq() ++ Frequency.empty[String]
        freq should === (testFreq())
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq() ++ Frequency("alpha" -> 14L, "epsilon" -> 7L)
        freq.get("alpha") should === (Some(24L))
        freq.get("epsilon") should === (Some(7L))
      }

      it("should accumulate multiple instances of the same domain value in the passed iterable") {
        val freq = testFreq() ++ Seq("alpha" -> 14L, "alpha" -> 7L)
        freq.get("alpha") should === (Some(31L))
      }

      it("Concat on empty shouldn't return a reference to the original mutable frequency") {
        val originalFreq = testFreq()
        val freq = originalFreq ++ Frequency.empty[String]
        originalFreq ++ Frequency("alpha" -> 14L, "epsilon" -> 7L)
        freq should === (testFreq())
        originalFreq should === (testFreq())
      }

    }

    describe(":++") {

      it("should be identity on an empty set") {
        val freq = testFreq() :++ Seq.empty[String]
        freq should === (testFreq())
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq() :++ Seq("alpha", "epsilon")
        freq.get("alpha") should === (Some(11L))
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (102L)
      }

      it("should accumulate multiple instances of the same domain value") {
        val freq = testFreq() :++ Seq("alpha", "alpha")
        freq.get("alpha") should === (Some(12L))
        freq.sum should === (102L)
      }

      it("should not alter the original frequency") {
        testFreq :++ Seq("alpha")
        testFreq().get("alpha") should === (Some(10L))
        testFreq().sum should === (100L)
      }

      it("Concat on empty shouldn't return a reference to the original mutable frequency") {
        val originalFreq = testFreq()
        val freq = originalFreq :++ Seq.empty[String]
        originalFreq :++ Seq("alpha", "epsilon")
        freq should === (testFreq())
        originalFreq should === (testFreq())
      }


    }

  }

  describe("mutable accumulation") {

    //It's important to use def so we get a new one each time we call it.
    def testFreq(): AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)

    describe("+=") {

      it("should accumulate on a new domain value") {
        val freq = testFreq()
        freq += ("epsilon" -> 7L)
        freq.get("epsilon") should === (Some(7L))
        freq.sum should === (107L)
      }

      it("should increment an existing domain value") {
        val freq = testFreq()
        freq += ("alpha" -> 14L)
        freq.get("alpha") should === (Some(24L))
        freq.sum should === (114L)
      }

    }

    describe("+:=") {

      it("should accumulate one count on a new domain value") {
        val freq = testFreq()
        freq :+= "epsilon"
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (101L)
      }

      it("should accumulate one count on an existing domain value") {
        val freq = testFreq()
        freq :+= "alpha"
        freq.get("alpha") should === (Some(11L))
        freq.sum should === (101L)
      }

    }

    describe("++=") {

      it("should be identity on an empty set") {
        val freq = testFreq()
        freq ++= Frequency.empty[String]
        freq should === (testFreq())
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq()
        freq ++= Frequency("alpha" -> 14L, "epsilon" -> 7L)
        freq.get("alpha") should === (Some(24L))
        freq.get("epsilon") should === (Some(7L))
      }

      it("should accumulate multiple instances of the same domain value in the passed iterable") {
        val freq = testFreq()
        freq ++= Seq("alpha" -> 14L, "alpha" -> 7L)
        freq.get("alpha") should === (Some(31L))
      }

    }

    describe(":++=") {

      it("should be identity on an empty set") {
        val freq = testFreq()
        freq :++ Seq.empty[String]
        freq should === (testFreq())
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq()
        freq :++= Seq("alpha", "epsilon")
        freq.get("alpha") should === (Some(11L))
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (102L)
      }

      it("should accumulate multiple instances of the same domain value") {
        val freq = testFreq()
        freq :++= Seq("alpha", "alpha")
        freq.get("alpha") should === (Some(12L))
        freq.sum should === (102L)
      }

    }

  }

  describe("conversion") {

    val testFreq: AtomicFrequencyBuffer[String] = AtomicFrequencyBuffer("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)

    describe("toMap") {

      it("should give back a map with the same pairs as the pairs method") {
        testFreq.toMap should === (Map("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L))
      }

    }

    describe("toSeq") {

      it("should give back the pairs, in no particular order") {
        testFreq.toSeq.sorted should === (Seq("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L).sorted)
      }

    }

    describe("toProbabilityDistribution") {

      it("should produce a probability distribution whose frequencies are the normalized frequencies") {
        val probDist = testFreq.toProbabilityDistribution

        val unnormalizedDist = probDist.pairs.map { pair =>
          (pair._1, pair._2.toBigDecimal * testFreq.sum)
        }
        val expectedPairs = testFreq.pairs.map { pair =>
          (pair._1, BigDecimal(pair._2))
        }

        unnormalizedDist should === (expectedPairs)
      }

    }

    describe("statistics") {

      it("should produce a statistics object with the same pairs as the frequency distribution") {
        val stats = testFreq.toStatistics
        stats.pairs should === (testFreq.pairs) //They should be identical, including sorting.
        stats.sum should === (100L)
      }

      it("Should produce a numerical domain statistics object") {
        val numFreq = AtomicFrequencyBuffer(2 -> 10L, 4 -> 30L)
        val numStats = numFreq.toNumericalDomainStatistics
        numStats.pairs should === (numFreq.pairs) //They should be identical, including sorting
        numStats.sum should === (40L)
        numStats.mean should === (3.5 +- ε)
      }

    }

  }

  it("should be thread safe") {
    val parallelization: Int = 16
    val bins: Int = 10
    val insertionsPerBinPerThread: Int = 100000

    val insertionsPerThread: Int = bins * insertionsPerBinPerThread
    val insertionsPerBin: Int = parallelization * insertionsPerBinPerThread
    val totalInsertions: Int = insertionsPerBin * bins

    val buffer = AtomicFrequencyBuffer.empty[Int]

    // We need high parallel write to a small set of bins
    (0 until parallelization).par.map { i =>
      (0 until insertionsPerThread).map { j =>
        buffer :+= (j % bins)
      }
    }
    
    buffer.sum should === (totalInsertions)
Ø
    (0 until bins).foreach { i =>
      assert(buffer.get(i) == (Some(insertionsPerBin)), s"incorrect count for bin index $i")
    }
  }

}
