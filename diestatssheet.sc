import scala.collection.immutable.SortedMap

object diestatssheet {

  class PDFCountBuffer[T] extends scala.collection.mutable.HashMap[T, Long] {

    def sum = foldLeft[Long](0)((acc, pair) => acc + pair._2)

    def toPDF = {
      val s = sum.toDouble
      //val m = map(pair => pair._1 -> (pair._2.toDouble/s))
      val m = mapValues(_.toDouble / s)
    }

  }

  object PDFCountBuffer {
    def empty[T] = new PDFCountBuffer[T]

    def apply[T](pairs: (T, Long)*): PDFCountBuffer[T] = {
      pairs.foldLeft(empty[T])((acc, pair) => acc += pair)
    }
  }

  //class PDF[T] extends scala.collection.immutable.Map[T,Double] {
  //}

  //type PDF[T] = SortedMap[T,Double]

  // TODO: It'd be better to take either a raw map or the raw pairs as the argument in the constructor,
  // because I need to normalize them and then put them in a SortedMap.
  // Example: SortedMap(someMap.toSeq: _*)
  //
  // It also seems as if this is actually a sequence to a casual user: It has a 0.0
  // value at most indices, but interestingly it is not a Seq as its template parameter
  // is on the keys, not on the values; and because it has can have any value as a key.
  class PDF[T](val coll: SortedMap[T, Double]) {

    def get(key: T): Option[Double] = coll.get(key)

    def iterator: Iterator[(T, Double)] = coll.iterator

    def +(kv: (T, Double)): PDF[T] = new PDF(coll + kv)

    def -(key: T): PDF[T] = new PDF(coll - key)

    override def toString = coll.toString
  }

  object PDF {
    def empty[T](implicit ord: Ordering[T]): PDF[T] = new PDF[T](SortedMap.empty(ord))

    def apply[T](elems: (T, Double)*)(implicit ord: Ordering[T]): PDF[T] = new PDF[T](SortedMap[T, Double](elems: _*))
  }

  val pdfb = PDFCountBuffer[Int](1 -> 7)
  pdfb += (3 -> 9)
  pdfb += (2 -> 3)
  pdfb
  pdfb.sum
  pdfb.toPDF


  //val ppp: PDF[Int] = SortedMap[Int, Double](3->7)

  val pdf1 = PDF.empty[Int]
  val pdf2 = PDF(3 -> 7)
}
