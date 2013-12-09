package net.paploo.diestats.test.pdf

import net.paploo.diestats.test.SpecTest
import net.paploo.diestats.pdf.Histogram

class HistogramTest extends SpecTest  {

  var hist = new Histogram[String]()

  override def beforeEach() {
    hist = new Histogram[String]()
    super.beforeEach()
  }

  describe("Histogram") {

    describe("default") {
      it("should return zero for unspecified keys") {
        val hist = new Histogram[String]()
        hist("eeloo") shouldEqual (0)
      }
    }

    describe("<<") {

      it("should increment the key by one when it is given just a key") {
        hist("Kerbin") shouldEqual (0)
        hist << "Kerbin"
        hist("Kerbin") shouldEqual (1)
        hist << "Kerbin"
        hist("Kerbin") shouldEqual (2)
      }

      it("should increment the key by count when it is given a pair") {
        hist("Kerbin") shouldEqual (0)
        hist << "Kerbin" -> 10
        hist("Kerbin") shouldEqual (10)
        hist << "Kerbin" -> 5
        hist("Kerbin") shouldEqual (15)
      }

      it("should allow chaining of appends") {
        hist("Kerbin") shouldEqual (0)
        hist << "Kerbin" << "Kerbin" -> 3 << "Kerbin"
        hist("Kerbin") shouldEqual (5)
      }
    }

    describe("compose") {

    }

    describe("toPDF") {

    }
  }

}
