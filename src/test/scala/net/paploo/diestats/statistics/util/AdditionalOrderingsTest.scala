package net.paploo.diestats.statistics.util

import net.paploo.diestats.test.SpecTest

class AdditionalOrderingsTest extends SpecTest {

  lazy val strings: Seq[Seq[Char]] = {
    val chars: Seq[Option[Char]] = Vector(None) ++ Vector('a', 'c', 'b', 'a').map(Some.apply)
    for {
      i <- chars
      j <- chars
      k <- chars
    } yield Seq(i, j, k).flatten
  }

  describe("SeqOrdering") {
    it("should compare two Seq[Char] using the same ordering as String") {

      val seqOrdering: Ordering[Seq[Char]] = AdditionalOrderings.SeqOrdering

      for {
        seq1 <- strings
        seq2 <- strings
      } {
        val str1 = seq1.mkString
        val str2 = seq2.mkString
        val seqOrder = seqOrdering.compare(seq1, seq2)
        val strOrder = str1 compare str2
        assert(Numeric.IntIsIntegral.signum(seqOrder) == Numeric.IntIsIntegral.signum(strOrder),
               s"(Expected ${Seq(seq1, seq2).sorted(seqOrdering)} to be in the order ${Seq(str1, str2).sorted})")
      }

    }

  }

}
