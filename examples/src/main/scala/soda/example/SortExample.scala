package soda.translator.example


trait SortExample {

  def is_sorted (sequence: Seq [Int]  ): Boolean
}

trait SortExampleWithAt  extends SortExample {

  def is_sorted (sequence: Seq [Int]  ): Boolean =
    sequence
      .indices
      .filter (index => index > 0 )
      .forall (index => sequence (index - 1 ) <= sequence (index )  )
}

case class SortExampleWithAt_ () extends SortExampleWithAt

trait SortExampleWithZip  extends SortExample {

  def is_sorted (sequence: Seq [Int]  ): Boolean =
    sequence
      .zip (sequence.tail )
      .forall (pair => (pair._1 <= pair._2 )  )
}

case class SortExampleWithZip_ () extends SortExampleWithZip

trait SortAlgorithmExample {

  def sort (sequence: Seq [Int]  ): Seq [Int]
}

trait SortAlgorithmExampleWithFold  extends SortAlgorithmExample {
  import soda.lib.Rec

  def sort (sequence: Seq [Int]  ): Seq [Int] =
    if (sequence.length < 2
    ) sequence
    else Rec () .fold (sequence, _initial_value, _next_value_function )

  lazy val _initial_value = Seq [Int]  ()

  def _next_value_function (current_sequence: Seq [Int], elem: Int ): Seq [Int] =
    insert_sorted (current_sequence, elem )

  def insert_sorted (sequence: Seq [Int], element: Int ): Seq [Int] =
    {
      lazy val first_part = sequence.takeWhile (x => x < element )
      lazy val middle = Seq (element )
      lazy val last_part = sequence.dropWhile (x => x < element )
      first_part.++ (middle.++ (last_part )  ) }
}

case class SortAlgorithmExampleWithFold_ () extends SortAlgorithmExampleWithFold

trait ConstrainedSortAlgorithm {
  import soda.lib.OptionSD
  import soda.lib.SomeElem
  import soda.lib.NoElem

  def sort (sequence: Seq [Int]  ): OptionSD [Seq [Int]] =
    {
      lazy val sorted_sequence =
        SortAlgorithmExampleWithFold_ () .sort (sequence )
      lazy val result =
        if (SortExampleWithZip_ () .is_sorted (sorted_sequence )
        ) SomeElem (sorted_sequence )
        else NoElem [Seq [Int]]  ()
      result }
}

case class ConstrainedSortAlgorithm_ () extends ConstrainedSortAlgorithm
