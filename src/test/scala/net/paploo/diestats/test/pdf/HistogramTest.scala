package net.paploo.diestats.test.pdf

import net.paploo.diestats.test.SpecTest
import net.paploo.diestats.pdf.Histogram

class HistogramTest extends SpecTest {

  var hist = new Histogram[String]()

  override def beforeEach(): Unit = {
    hist = new Histogram[String]()
    super.beforeEach()
  }

  describe("Histogram") {

    describe("default") {
      it("should return zero for unspecified keys") {
        val hist = new Histogram[String]()
        hist("eeloo") shouldEqual 0
      }
    }

    describe("<<") {

      it("should increment the key by one when it is given just a key") {
        hist("Kerbin") shouldEqual 0
        hist << "Kerbin"
        hist("Kerbin") shouldEqual 1
        hist << "Kerbin"
        hist("Kerbin") shouldEqual 2
      }

      it("should increment the key by count when it is given a pair") {
        hist("Kerbin") shouldEqual 0
        hist << "Kerbin" -> 10
        hist("Kerbin") shouldEqual 10
        hist << "Kerbin" -> 5
        hist("Kerbin") shouldEqual 15
      }

      it("should allow chaining of appends") {
        hist("Kerbin") shouldEqual 0
        hist << "Kerbin" << "Kerbin" -> 3 << "Kerbin"
        hist("Kerbin") shouldEqual 5
      }
    }

    describe("compose") {
      val hist1 = new Histogram[String]() << "Eve" -> 5 << "Kerbin" -> 5
      val hist2 = new Histogram[String]() << "Kerbin" -> 6 << "Duna" -> 6

      val hist12 = hist1 compose hist2

      it("should mutate the lhs histogram") {
        hist1("Eve").shouldBe(5)
        hist1("Kerbin").shouldBe(11)
        hist1("Duna").shouldBe(6)
      }

      it("should leave the rhs histogram untouched") {
        hist2("Eve").shouldBe(0)
        hist2("Kerbin").shouldBe(6)
        hist2("Duna").shouldBe(6)
      }

      it("should return a reference to the lhs") {
        assert(hist12 eq hist1)
      }
    }

    describe("toPDF") {
      it("should convert an array with int keys to a PDF") {
        val h = new Histogram[Int]() << 1 << 1 << 2 << 3
        val pdf = h.toPDF

        pdf(1) shouldEqual 0.50
        pdf(2) shouldEqual 0.25
        pdf(3) shouldEqual 0.25
      }
    }
  }

}
