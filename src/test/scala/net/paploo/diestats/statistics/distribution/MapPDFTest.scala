package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.domain.DomainOperations
import net.paploo.diestats.test.SpecTest

class MapPDFTest extends SpecTest {

  val pdf = MapPDF(Map(1 -> 0.25, 3 -> 0.75))

  val expectedDomain = Seq(1, 3)
  val pexpectedPairs = Seq((1, 0.25), (3, 0.75))

  describe("creation from counts") {

    val pdf = PDF.fromCounts(Map(1 -> 100L, 2 -> 200L, 3 -> 300L))

    it("should be normalized") {
      pdf.pairs.map(_._2).sum should === (1.0 +- ε)
    }

  }

  describe("apply") {

    it("should return the probability associated with the key") {
      pdf(1) should === (0.25 +- ε)
    }

    it("should return 0.0 if accessing an undefined key") {
      pdf(-1) should === (0.0 +- ε)
    }

  }

  describe("get") {

    it("should return the an option of the probability associated with the key") {
      pdf.get(1).get should === (0.25 +- ε)
    }

    it("should return None") {
      pdf.get(-1) should === (None)
    }

  }

  describe("domain") {

    it("should return the domain of defined keys") {
      pdf.domain should === (expectedDomain)
    }

    it("should be sortable") {
      pdf.domain(Ordering.by(-_)) should === (expectedDomain.reverse)
    }

  }

  describe("pairs") {

    it("should return the domain of defined keys") {
      pdf.pairs should === (pexpectedPairs)
    }

    it("should be sortable") {
      pdf.pairs(Ordering.by(-_)) should === (pexpectedPairs.reverse)
    }

  }

  describe("convolve") {

    val pdf1 = MapPDF(Map(1 -> 0.25, 3 -> 0.75))
    val pdf2 = MapPDF(Map(1 -> 0.25, 5 -> 0.75))

    val expectedPdf = MapPDF(Map(2 -> 0.0625, 4 -> 0.1875, 6 -> 0.1875, 8 -> 0.5625))

    it("should convolve") {
      import DomainOperations.Implicits._

      (pdf1 convolve pdf2).pairs should === (expectedPdf.pairs)
    }

    it("should convolve with an alternate domainOp") {
      val domainOp = new DomainOperations[Int] {
        override def concat(x: Int, y: Int): Int = x * y
      }

      val expectedPdf = MapPDF(Map(1 -> 0.0625, 3 -> 0.1875, 5 -> 0.1875, 15 -> 0.5625))

      pdf1.convolve(pdf2)(domainOp).pairs should === (expectedPdf.pairs)
    }

  }

  describe("toCDF") {

  }

}
