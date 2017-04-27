package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.test.SpecTest

class FrequencyMapTest extends SpecTest {

  describe("object methods") {

    it("should construct an empty frequency") {
      val freq: FrequencyMap[String] = FrequencyMap.empty[String]
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct an empty frequency from apply with no args") {
      val freq: FrequencyMap[String] = FrequencyMap[String]()
      freq.sum should === (0L)
      freq.domain should === (Seq.empty[String])
      freq.pairs should === (Seq.empty[(String, Long)])
    }

    it("should construct from pairs") {
      val freq: FrequencyMap[String] = FrequencyMap("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should construct from values") {
      val freq: FrequencyMap[String] = FrequencyMap.fromValues("alpha", "beta", "alpha", "gamma")
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("Should build from an iterable of pairs") {
      val freq: FrequencyMap[String] = FrequencyMap.buildFrom(Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should build an iterable of values") {
      val freq: FrequencyMap[String] = FrequencyMap.buildFromValues(Seq("alpha", "beta", "alpha", "gamma"))
      freq.sum should === (4L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
      freq.pairs should === (Seq(("alpha", 2L), ("beta", 1L), ("gamma", 1L)))
    }

    it("should create empty with a domain") {
      val freq: FrequencyMap[String] = FrequencyMap.emptyWithDomain("alpha", "beta", "gamma")
      freq.sum should === (0L)
      freq.domain should === (Seq("alpha", "beta", "gamma"))
    }

  }

  describe("implicit iterable conversion") {

    val freq: FrequencyMap[String] = FrequencyMap("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)
    val freqSeq = Seq("alpha" -> 2L, "beta" -> 1L, "gamma" -> 1L)

    it("should implicitly convert to an iterable") {
      val iterable: Iterable[(String, Long)] = freq
      iterable.toSeq.sorted should === (freqSeq.sorted)
    }

  }

  describe("basic properties") {

    val testFreq: FrequencyMap[String] = FrequencyMap("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)
    val reverseOrdering: Ordering[String] = Ordering.String.reverse

    describe("get") {

      it("should return Some(count) for a counted domain value") {
        testFreq.get("gamma") should === (Some(20))
      }

      it("should return None for a not counted domain value") {
        testFreq.get("zeta") should === (None)
      }

    }

    describe("count") {

      it("should return the total count") {
        testFreq.sum should === (100)
      }

    }

    describe("domain") {

      it("should return the domain using the default sorting") {
        testFreq.domain should === (Seq("alpha", "beta", "delta", "gamma"))
      }

      it("should sort the domain according to the scoped implicit ordering") {
        implicit val ord = reverseOrdering
        testFreq.domain should === (Seq("gamma", "delta", "beta", "alpha"))
      }

    }

    describe("pairs") {

      it("should return the pairs using the default sorting") {
        testFreq.pairs should === (Seq("alpha" -> 10L, "beta" -> 40L, "delta" -> 30L, "gamma" -> 20L))
      }

      it("should sort the pairs according ot the implicit domain ordering") {
        implicit val ord = reverseOrdering
        testFreq.pairs should === (Seq("gamma" -> 20L, "delta" -> 30L, "beta" -> 40L, "alpha" -> 10L))
      }

    }


  }

  describe("accumulation") {

    val testFreq: FrequencyMap[String] = FrequencyMap("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)

    describe("+") {

      it("should accumulate on a new domain value") {
        val freq = testFreq + ("epsilon" -> 7L)
        freq.get("epsilon") should === (Some(7L))
        freq.sum should === (107L)
      }

      it("should increment an existing domain value") {
        val freq = testFreq + ("alpha" -> 14L)
        freq.get("alpha") should === (Some(24L))
        freq.sum should === (114L)
      }

      it("should not alter the original frequency") {
        testFreq + ("epsilon" -> 7L)
        testFreq.get("epsilon") should === (None)
        testFreq.sum should === (100L)
      }

    }

    describe("+:") {

      it("should accumulate one count on a new domain value") {
        val freq = testFreq :+ "epsilon"
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (101L)
      }

      it("should accumulate one count on an existing domain value") {
        val freq = testFreq :+ "alpha"
        freq.get("alpha") should === (Some(11L))
        freq.sum should === (101L)
      }

      it("should not alter the original frequency") {
        testFreq :+ "alpha"
        testFreq.get("alpha") should === (Some(10L))
        testFreq.sum should === (100L)
      }

    }

    describe("++") {

      it("should be identity on an empty set") {
        //TODO: On mutable frequencies, we should test that mutating what's returned from this doesn't mutate the original!
        val freq = testFreq ++ Frequency.empty[String]
        freq should === (testFreq)
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq ++ Frequency("alpha" -> 14L, "epsilon" -> 7L)
        freq.get("alpha") should === (Some(24L))
        freq.get("epsilon") should === (Some(7L))
      }

      it("should accumulate multiple instances of the same domain value in the passed iterable") {
        val freq = testFreq ++ Seq("alpha" -> 14L, "alpha" -> 7L)
        freq.get("alpha") should === (Some(31L))
      }

      it("should not alter the original frequency") {
        testFreq ++ Frequency("alpha" -> 14L)
        testFreq.get("alpha") should === (Some(10L))
        testFreq.sum should === (100L)
      }

    }

    describe(":++") {

      it("should be identity on an empty set") {
        //TODO: On mutable frequencies, we should test that mutating what's returned from this doesn't mutate the original!
        val freq = testFreq :++ Seq.empty[String]
        freq should === (testFreq)
      }

      it("should accept both new and existing domain values") {
        val freq = testFreq :++ Seq("alpha", "epsilon")
        freq.get("alpha") should === (Some(11L))
        freq.get("epsilon") should === (Some(1L))
        freq.sum should === (102L)
      }

      it("should accumulate multiple instances of the same domain value") {
        val freq = testFreq :++ Seq("alpha", "alpha")
        freq.get("alpha") should === (Some(12L))
        freq.sum should === (102L)
      }

      it("should not alter the original frequency") {
        testFreq :++ Seq("alpha")
        testFreq.get("alpha") should === (Some(10L))
        testFreq.sum should === (100L)
      }

    }

  }

  describe("conversion") {

    val testFreq: FrequencyMap[String] = FrequencyMap("alpha" -> 10L, "beta" -> 40L, "gamma" -> 20L, "delta" -> 30L)

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
        val numFreq = FrequencyMap(2 -> 10L, 4 -> 30L)
        val numStats = numFreq.toNumericalDomainStatistics
        numStats.pairs should === (numFreq.pairs) //They should be identical, including sorting
        numStats.sum should === (40L)
        numStats.mean should === (3.5 +- ε)
      }

    }

  }

}
