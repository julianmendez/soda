trait SortExample
{

  def   is_sorted : Seq [Int] => Boolean

}

case class SortExample_ (is_sorted : Seq [Int] => Boolean) extends SortExample

object SortExample { def mk  (is_sorted : Seq [Int] => Boolean) : SortExample  = SortExample_  (is_sorted) }

trait SortExampleWithAt
  extends
    SortExample
{

  def is_sorted_for (sequence : Seq [Int] ) : Boolean =
    sequence
      .indices
      .filter ( index => index > 0)
      .forall ( index => sequence .apply (index - 1) <= sequence .apply (index) )

  lazy val is_sorted : Seq [Int] => Boolean =
     sequence => is_sorted_for (sequence)

}

case class SortExampleWithAt_ () extends SortExampleWithAt

object SortExampleWithAt { def mk   : SortExampleWithAt  = SortExampleWithAt_  () }

trait SortExampleWithZip
  extends
    SortExample
{

  def is_sorted_for (sequence : Seq [Int] ) : Boolean =
    sequence
      .zip (sequence .tail)
      .forall ( pair => (pair ._1 <= pair ._2) )

  lazy val is_sorted : Seq [Int] => Boolean =
     sequence => is_sorted_for (sequence)

}

case class SortExampleWithZip_ () extends SortExampleWithZip

object SortExampleWithZip { def mk   : SortExampleWithZip  = SortExampleWithZip_  () }

trait SortAlgorithmExample
{

  def   sort : Seq [Int] => Seq [Int]

}

case class SortAlgorithmExample_ (sort : Seq [Int] => Seq [Int]) extends SortAlgorithmExample

object SortAlgorithmExample { def mk  (sort : Seq [Int] => Seq [Int]) : SortAlgorithmExample  = SortAlgorithmExample_  (sort) }

trait SortAlgorithmExampleWithFold
  extends
    SortAlgorithmExample
{

  import   soda.lib.Fold_

  private lazy val _fold = Fold_ ()

  private lazy val _initial_value = Seq [Int] ()

  private def _next_value_function (current_sequence : Seq [Int] ) (elem : Int) : Seq [Int] =
    insert_sorted (current_sequence) (elem)

  def sort_for (sequence : Seq [Int] ) : Seq [Int] =
    if ( sequence .length < 2
    ) sequence
    else _fold .apply (sequence) (_initial_value) (_next_value_function)

  lazy val sort : Seq [Int] => Seq [Int] =
     sequence => sort_for (sequence)

  def concatenate (first_part : Seq [Int] ) (middle : Seq [Int] ) (last_part : Seq [Int] )
      : Seq [Int] =
    first_part .++ (middle .++ (last_part) )

  def insert_sorted (sequence : Seq [Int] ) (element : Int) : Seq [Int] =
    concatenate (
      first_part = sequence .takeWhile ( x => x < element) ) (
      middle = Seq (element) ) (
      last_part = sequence .dropWhile ( x => x < element)
    )

}

case class SortAlgorithmExampleWithFold_ () extends SortAlgorithmExampleWithFold

object SortAlgorithmExampleWithFold { def mk   : SortAlgorithmExampleWithFold  = SortAlgorithmExampleWithFold_  () }

trait ConstrainedSortAlgorithm
{

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.lib.NoneSD_

  def sort_with (sorted_sequence : Seq [Int] ) : OptionSD [Seq [Int] ] =
    if ( SortExampleWithZip_ () .is_sorted (sorted_sequence)
    ) SomeSD_ (sorted_sequence)
    else NoneSD_ [Seq [Int] ] ()

  def sort (sequence : Seq [Int] ) : OptionSD [Seq [Int] ] =
    sort_with (SortAlgorithmExampleWithFold_ () .sort (sequence) )

}

case class ConstrainedSortAlgorithm_ () extends ConstrainedSortAlgorithm

object ConstrainedSortAlgorithm { def mk   : ConstrainedSortAlgorithm  = ConstrainedSortAlgorithm_  () }

trait SortedSequence [A <: Comparable [A] ]
{

  def   sequence : Seq [A]
  def   add : A => SortedSequence [A]
  def   invariant : Boolean

}

case class SortedSequence_ [A <: Comparable [A]] (sequence : Seq [A], add : A => SortedSequence [A], invariant : Boolean) extends SortedSequence [A]

object SortedSequence { def mk  [A <: Comparable [A]] (sequence : Seq [A]) (add : A => SortedSequence [A]) (invariant : Boolean) : SortedSequence  [A] = SortedSequence_  [A] (sequence, add, invariant) }

trait EmptySortedSequence [A <: Comparable [A] ]
  extends
    SortedSequence [A]
{

  lazy val sequence = Seq ()

  def add_for (element : A) : SortedSequence [A] =
    NonEmptySortedSequence_ (Seq (element) )

  lazy val add : A => SortedSequence [A] =
     element => add_for (element)

  lazy val invariant : Boolean = true

}

case class EmptySortedSequence_ [A <: Comparable [A]] () extends EmptySortedSequence [A]

object EmptySortedSequence { def mk  [A <: Comparable [A]]  : EmptySortedSequence  [A] = EmptySortedSequence_  [A] () }

trait SortedSequenceWithElements [A <: Comparable [A] ]
  extends
    SortedSequence [A]
{

  def   sequence : Seq [A]
  def   add : A => SortedSequence [A]
  def   invariant : Boolean

}

case class SortedSequenceWithElements_ [A <: Comparable [A]] (sequence : Seq [A], add : A => SortedSequence [A], invariant : Boolean) extends SortedSequenceWithElements [A]

object SortedSequenceWithElements { def mk  [A <: Comparable [A]] (sequence : Seq [A]) (add : A => SortedSequence [A]) (invariant : Boolean) : SortedSequenceWithElements  [A] = SortedSequenceWithElements_  [A] (sequence, add, invariant) }

trait NonEmptySortedSequence [A <: Comparable [A] ]
  extends
    SortedSequenceWithElements [A]
{

  def   sequence : Seq [A]

  lazy val aux = NonEmptySortedSequenceAux_ [A] ()

  def add_for (element : A) : SortedSequence [A] =
    NonEmptySortedSequence_ (aux .insert_sorted (sequence) (element) )

  lazy val add : A => SortedSequence [A] =
     element => add_for (element)

  lazy val invariant : Boolean = aux .is_sorted (sequence)

}

case class NonEmptySortedSequence_ [A <: Comparable [A]] (sequence : Seq [A]) extends NonEmptySortedSequence [A]

object NonEmptySortedSequence { def mk  [A <: Comparable [A]] (sequence : Seq [A]) : NonEmptySortedSequence  [A] = NonEmptySortedSequence_  [A] (sequence) }

trait NonEmptySortedSequenceAux [A <: Comparable [A] ]
{

  def is_less_than (x : A) (y : A) : Boolean =
    x .compareTo (y) < 0

  def is_sorted (other_sequence : Seq [A] ) : Boolean =
    other_sequence
      .zip (other_sequence .tail)
      .forall ( pair => is_less_than (pair ._1) (pair ._2) )

  def concatenate (first_part : Seq [A] ) (middle : Seq [A] ) (last_part : Seq [A] ) : Seq [A] =
    first_part .++ (middle .++ (last_part) )

  def insert_sorted (original_sequence : Seq [A] ) (element : A) : Seq [A] =
    concatenate (
      first_part = original_sequence.takeWhile ( x => is_less_than (x) (element) ) ) (
      middle = Seq (element) ) (
      last_part = original_sequence.dropWhile ( x => is_less_than (x) (element) )
    )

}

case class NonEmptySortedSequenceAux_ [A <: Comparable [A]] () extends NonEmptySortedSequenceAux [A]

object NonEmptySortedSequenceAux { def mk  [A <: Comparable [A]]  : NonEmptySortedSequenceAux  [A] = NonEmptySortedSequenceAux_  [A] () }

trait SortedSequenceBuilder [A <: Comparable [A] ]
{

  import   soda.lib.Fold_

  private lazy val _fold = Fold_ ()

  private lazy val _initial_value : SortedSequence [A] = EmptySortedSequence_ [A] ()

  private def _next_value_function (sorted_sequence : SortedSequence [A] ) (element : A)
      : SortedSequence [A] =
    sorted_sequence .add (element)

  def build (sequence : Seq [A] ) : SortedSequence [A] =
    _fold .apply (sequence) (_initial_value) (_next_value_function)

}

case class SortedSequenceBuilder_ [A <: Comparable [A]] () extends SortedSequenceBuilder [A]

object SortedSequenceBuilder { def mk  [A <: Comparable [A]]  : SortedSequenceBuilder  [A] = SortedSequenceBuilder_  [A] () }
