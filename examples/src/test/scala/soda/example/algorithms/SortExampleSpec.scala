package soda.example.algorithms

case class SortExampleSpec ()
  extends org.scalatest.funsuite.AnyFunSuite {

  import   soda.lib.SomeSD_

  test ("test sorted sequence with at")
    {
      lazy val sorted_sequence = Seq (1, 3, 5, 5, 8, 9 )
      lazy val expected = true
      lazy val obtained = SortExampleWithAt_ () .is_sorted (sorted_sequence )
      assert (obtained == expected ) }

  test ("test unsorted sequence with at")
    {
      lazy val unsorted_sequence = Seq (1, 3, 5, 4, 8, 9 )
      lazy val expected = false
      lazy val obtained = SortExampleWithAt_ () .is_sorted (unsorted_sequence )
      assert (obtained == expected ) }

  test ("test sorted sequence with zip")
    {
      lazy val sorted_sequence = Seq (1, 3, 5, 5, 8, 9 )
      lazy val expected = true
      lazy val obtained = SortExampleWithZip_ () .is_sorted (sorted_sequence )
      assert (obtained == expected ) }

  test ("test unsorted sequence with zip")
    {
      lazy val unsorted_sequence = Seq (1, 3, 5, 4, 8, 9 )
      lazy val expected = false
      lazy val obtained = SortExampleWithZip_ () .is_sorted (unsorted_sequence )
      assert (obtained == expected ) }

  test ("insert sorted simple")
    {
      lazy val instance = SortAlgorithmExampleWithFold_ ()
      lazy val sorted_sequence = Seq (1, 2, 3, 6, 8, 9 )
      lazy val expected = Seq (1, 2, 3, 5, 6, 8, 9 )
      lazy val obtained = instance.insert_sorted (sorted_sequence, 5 )
      assert (obtained == expected ) }

  test ("insert sorted with repetition")
    {
      lazy val instance = SortAlgorithmExampleWithFold_ ()
      lazy val sorted_sequence = Seq (1, 2, 3, 5, 6, 8, 9 )
      lazy val expected = Seq (1, 2, 3, 5, 5, 6, 8, 9 )
      lazy val obtained = instance.insert_sorted (sorted_sequence, 5 )
      assert (obtained == expected ) }

  test ("sort unsorted sequence")
    {
      lazy val instance = SortAlgorithmExampleWithFold_ ()
      lazy val unsorted_sequence = Seq (3, 5, 1, 9, 8, 4 )
      lazy val expected = Seq (1, 3, 4, 5, 8, 9 )
      lazy val obtained = instance.sort (unsorted_sequence )
      assert (obtained == expected ) }

  test ("sort unsorted sequence applying constraints to verify correctness")
    {
      lazy val instance = ConstrainedSortAlgorithm_ ()
      lazy val unsorted_sequence = Seq (3, 5, 1, 9, 8, 4 )
      lazy val expected = SomeSD_ (Seq (1, 3, 4, 5, 8, 9 )  )
      lazy val obtained = instance.sort (unsorted_sequence )
      assert (obtained == expected ) }

  test ("sort unsorted sequence with SortedSequenceBuilder")
    {
      lazy val instance = SortedSequenceBuilder_ [Integer] ()
      lazy val unsorted_sequence = Seq (3, 5, 1, 9, 8, 4 ) .map (x => Integer.valueOf (x )  )
      lazy val expected = Seq (1, 3, 4, 5, 8, 9 )
      lazy val obtained =
        instance
          .build (unsorted_sequence )
          .sequence
          .map (x => x.intValue )
      assert (obtained == expected ) }

}
