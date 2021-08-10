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

trait SortedSequence [A <: Comparable [A]] {
  lazy val doc_SortedSequence = "This models a sequence that is always sorted."

  def sequence: Seq [A]

  def add (element: A ): SortedSequence [A]

  def is_less_than (x: A, y: A ): Boolean =
    x.compareTo (y ) < 0

  def is_sorted (other_sequence: Seq [A]  ): Boolean =
    other_sequence
      .zip (other_sequence.tail )
      .forall (pair => is_less_than (pair._1, pair._2 )  )

  lazy val invariant: Boolean = is_sorted (sequence )
}

trait EmptySortedSequence [A <: Comparable [A]]  extends SortedSequence [A] {

  lazy val sequence = Seq ()

  def add (element: A ): SortedSequence [A] =
     _NonEmptySortedSequence_ (Seq (element )  )
}

case class EmptySortedSequence_ [A <: Comparable [A]]  ()  extends EmptySortedSequence [A]

trait NonEmptySortedSequence [A <: Comparable [A]]  extends SortedSequence [A] {

  def sequence: Seq [A]

  def add (element: A ): SortedSequence [A] =
    _NonEmptySortedSequence_ (_insert_sorted (sequence, element )  )

  def _insert_sorted (original_sequence: Seq [A], element: A ): Seq [A] =
    {
      lazy val first_part =
        original_sequence.takeWhile (x => is_less_than (x, element )  )

      lazy val middle = Seq (element )

      lazy val last_part =
        original_sequence.dropWhile (x => is_less_than (x, element )  )
      first_part.++ (middle.++ (last_part )  ) }
}

case class _NonEmptySortedSequence_ [A <: Comparable [A]]  (sequence: Seq [A]  )  extends NonEmptySortedSequence [A]

trait SortedSequenceBuilder [A <: Comparable [A]] {
  import soda.lib.Rec

  def build (sequence: Seq [A]  ): SortedSequence [A] =
    Rec () .fold (sequence, _initial_value, _next_value_function )

  lazy val _initial_value = EmptySortedSequence_ [A]  ()

  def _next_value_function (sorted_sequence: SortedSequence [A], element: A ): SortedSequence [A] =
    sorted_sequence.add (element )
}

case class SortedSequenceBuilder_ [A <: Comparable [A]]  () extends SortedSequenceBuilder [A]
