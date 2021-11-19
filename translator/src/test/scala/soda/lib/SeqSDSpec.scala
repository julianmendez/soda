package soda.lib


case class SeqSDSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should detect an empty sequence") {
    lazy val input = Seq ()
    lazy val seqsd = SeqSDBuilder_ () .build (input )
    lazy val obtained = seqsd.opt (ifEmpty = true, ifNonEmpty = nonEmpty => false    )

    assert (obtained )

}

  test ("should detect an non empty sequence") {
    lazy val input = Seq (1 )
    lazy val seqsd = SeqSDBuilder_ () .build (input )
    lazy val obtained = seqsd.opt (ifEmpty = false, ifNonEmpty = nonEmpty => true    )

    assert (obtained )

}

  test ("should get the maximum") {
    def max_of_2 (a: Int, b: Int ): Int = if (a > b ) a else b
    def max (s: NonEmptySeqSD [Int]  ): Int =
      Recursion_ () .fold (s.tail.toSeq, s.head, max_of_2 )
    lazy val input = Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 )
    lazy val expected = SomeSD_ [Int]  (9 )
    lazy val obtained =
      SeqSDBuilder_ () .build (input ) .opt (ifEmpty = NoneSD_ (), ifNonEmpty = sequence => SomeSD_ (max (sequence )  )      )

    assert (obtained == expected )

}

  test ("should reverse a sequence") {
    lazy val input = SeqSDBuilder_ () .build (Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 ) )
    lazy val expected = SeqSDBuilder_ () .build (Seq (9, 5, 4, 8, 2, 8, 1, 8, 2, 8, 1, 7, 2 ) )
    lazy val obtained = input.reverse

    assert (obtained == expected )

}

  test ("should reverse another sequence") {
    lazy val input = SeqSDBuilder_ () .build (Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9 ) )
    lazy val expected = SeqSDBuilder_ () .build (Seq (9, 5, 4, 8, 2, 8, 1, 8, 2, 8, 1, 7, 2 ) )
    lazy val obtained = input.opt (ifEmpty = EmptySeqSD_ (), ifNonEmpty = sequence => sequence.reverse    )

    assert (obtained == expected )

}

}
