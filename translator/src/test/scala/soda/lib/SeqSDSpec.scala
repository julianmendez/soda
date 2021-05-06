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

    def max (s: NonEmptySeqSD [Int]  ): Int =
      Rec () .foldLeft (s.tail.toSeq, s.head, max_of_2 )

    lazy val input = Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 )
    lazy val expected = SomeSD [Int]  (9 )
    lazy val obtained =
      SeqSDBuilder () .build (input ) .open (
        ifEmpty = NoneSD (), ifNonEmpty = sequence => SomeSD (max (sequence )  )
      )
    assert (obtained == expected )
  }

  test ("should reverse a sequence") {
    lazy val input = SeqSDBuilder () .build (Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 ) )
    lazy val expected = SeqSDBuilder () .build (Seq (9, 5, 4, 8, 2, 8, 1, 8, 2, 8, 1, 7, 2 ) )
    lazy val obtained = input.reverse
    assert (obtained == expected )
  }

  test ("should reverse another sequence") {
    lazy val input = SeqSDBuilder () .build (Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 ) )
    lazy val expected = SeqSDBuilder () .build (Seq (9, 5, 4, 8, 2, 8, 1, 8, 2, 8, 1, 7, 2 ) )
    lazy val obtained = input.open (
      ifEmpty = EmptySeqSD (), ifNonEmpty = sequence => sequence.reverse
    )
    assert (obtained == expected )
  }

}
