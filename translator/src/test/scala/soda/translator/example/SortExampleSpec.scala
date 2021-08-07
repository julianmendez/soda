package soda.translator.example


case class SortExampleSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("test sorted sequence with at") {
    lazy val sorted_sequence = Seq (1, 3, 5, 5, 8, 9 )
    lazy val expected = true
    lazy val obtained = SortExampleNaive_ () .is_sorted_with_at (sorted_sequence )

    assert (obtained == expected )
  }

  test ("test unsorted sequence with at") {
    lazy val sorted_sequence = Seq (1, 3, 5, 4, 8, 9 )
    lazy val expected = false
    lazy val obtained = SortExampleNaive_ () .is_sorted_with_at (sorted_sequence )

    assert (obtained == expected )
  }
  test ("test sorted sequence with zip") {
    lazy val sorted_sequence = Seq (1, 3, 5, 5, 8, 9 )
    lazy val expected = true
    lazy val obtained = SortExampleNaive_ () .is_sorted_with_zip (sorted_sequence )

    assert (obtained == expected )
  }

  test ("test unsorted sequence with zip") {
    lazy val sorted_sequence = Seq (1, 3, 5, 4, 8, 9 )
    lazy val expected = false
    lazy val obtained = SortExampleNaive_ () .is_sorted_with_zip (sorted_sequence )

    assert (obtained == expected )
  }
}
