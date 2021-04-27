package soda.lib

import org.scalatest.funsuite.AnyFunSuite

case class SeqSDSpec () extends AnyFunSuite {


  test ("should detect an empty sequence") {
    lazy val input = Seq ()
    lazy val seqsd = SeqSDBuilder () .build (input )
    lazy val obtained = seqsd.open (
      ifEmpty = true, ifNonEmpty = nonEmpty => false
    )
    assert (obtained )
  }

  test ("should detect an non empty sequence") {
    lazy val input = Seq (1 )
    lazy val seqsd = SeqSDBuilder () .build (input )
    lazy val obtained = seqsd.open (
      ifEmpty = false, ifNonEmpty = nonEmpty => true
    )
    assert (obtained )
  }

  test ("should get the maximum") {
    def max_of_2 (a: Int, b: Int ): Int = if (a > b ) a else b
    def max (s: NESeqSD [Int]  ): Int =
      Rec () .foldLeft (s.tailSeq, s.head, max_of_2 )

    lazy val input = Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 )
    lazy val expected = 9
    lazy val obtained = max (NESeqSD [Int]  (input.head, input.tail )  )
    assert (obtained == expected )
  }

}
