package soda.lib


case class CombSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("Cartesian product of two sequences") {
    lazy val expected = Seq (Seq (10, 100 ), Seq (10, 200 ), Seq (10, 300 ), Seq (20, 100 ), Seq (20, 200 ), Seq (20, 300 )
    )

    lazy val seq_a = Seq (10, 20 )
    lazy val seq_b = Seq (100, 200, 300 )

    lazy val obtained = Comb () .cartesianProduct (Seq (seq_a, seq_b )  )

    assert (obtained == expected )
  }

  test ("Cartesian product of an empty sequence") {
    lazy val expected = Seq ()
    lazy val obtained = Comb () .cartesianProduct (Seq ()  )
    assert (obtained == expected )
  }

  test ("Cartesian product of only empty sequences") {
    lazy val expected = Seq ()
    lazy val obtained = Comb () .cartesianProduct (Seq (Seq (), Seq (), Seq ()  )  )
    assert (obtained == expected )
  }

  test ("Cartesian product with at least one empty sequence") {
    lazy val expected = Seq ()
    lazy val obtained = Comb () .cartesianProduct (Seq (Seq ("A"), Seq ()  )  )
    assert (obtained == expected )
  }

  test ("Cartesian product of three sequences") {
    lazy val expected = Seq (Seq ("A", "0", "a"), Seq ("A", "0", "b"), Seq ("A", "0", "c"), Seq ("A", "0", "d"), Seq ("A", "1", "a"), Seq ("A", "1", "b"), Seq ("A", "1", "c"), Seq ("A", "1", "d"), Seq ("A", "2", "a"), Seq ("A", "2", "b"), Seq ("A", "2", "c"), Seq ("A", "2", "d"), Seq ("B", "0", "a"), Seq ("B", "0", "b"), Seq ("B", "0", "c"), Seq ("B", "0", "d"), Seq ("B", "1", "a"), Seq ("B", "1", "b"), Seq ("B", "1", "c"), Seq ("B", "1", "d"), Seq ("B", "2", "a"), Seq ("B", "2", "b"), Seq ("B", "2", "c"), Seq ("B", "2", "d")
    )

    lazy val seq_a = Seq ("A", "B")
    lazy val seq_b = Seq ("0", "1", "2")
    lazy val seq_c = Seq ("a", "b", "c", "d")

    lazy val obtained = Comb () .cartesianProduct (Seq (seq_a, seq_b, seq_c )  )

    assert (obtained == expected )
  }


}
