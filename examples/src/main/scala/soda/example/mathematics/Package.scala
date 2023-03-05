package soda.example.mathematics

/*
 * This package contains examples in Soda.
 * These examples use mathematical properties.
 */

trait Package

trait FactorialConcise
{

  def apply (n : Int) : Int =
    _tailrec_get_factorial (n) (1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    if ( n == 0
    ) product
    else _tailrec_get_factorial (n - 1) (n * product)

}

case class FactorialConcise_ () extends FactorialConcise


trait FactorialPatternMatching
{

  def apply (n : Int) : Int =
    _tailrec_get_factorial (n) (1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    n match  {
      case 0 => product
      case x => _tailrec_get_factorial (x - 1) (x * product)
    }

}

case class FactorialPatternMatching_ () extends FactorialPatternMatching


trait FactorialWithFold
{

  private lazy val _fold = soda.lib.Fold_ ()

  private lazy val _range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    _fold.apply (_range.apply (n) ) (1) ( product =>  k => (product *  (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold


trait FiboExampleInSoda
{

  def apply (n : Int) : Int =
    _rec (n) (0) (1)

  private def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
    else if ( m == 1 ) b
    else _rec (m - 1) (b) (a + b)

}

case class FiboExampleInSoda_ () extends FiboExampleInSoda


trait FiboUnicodeExample
{

  def apply (n : Int) : Int =
    _rec (n) (0) (1)

  private def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
     else if ( m == 1 ) b
        else _rec (m - 1) (b) (a + b)

}

case class FiboUnicodeExample_ () extends FiboUnicodeExample


trait InputPair [A, B]
{

  def   value : A
  def   memoized_values : Map [A, B]

}

case class InputPair_ [A, B] (value : A, memoized_values : Map [A, B]) extends InputPair [A, B]

trait OutputPair [A, B]
{

  def   value : B
  def   memoized_values : Map [A, B]

}

case class OutputPair_ [A, B] (value : B, memoized_values : Map [A, B]) extends OutputPair [A, B]

trait MemoizableFunction [A, B]
{

    /** compute (input : InputPair [A, B] ) : OutputPair [A, B] */
  def   abs_compute : InputPair [A, B] => OutputPair [A, B]

  def compute (input : InputPair [A, B] ) : OutputPair [A, B] =
    abs_compute (input)

}

case class MemoizableFunction_ [A, B] (abs_compute : InputPair [A, B] => OutputPair [A, B]) extends MemoizableFunction [A, B]

trait MainFunction [A, B]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

}

case class MainFunction_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends MainFunction [A, B]

trait Memoizer [A, B]
  extends
    MemoizableFunction [A, B]
    with MainFunction [A, B]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

  lazy val abs_compute : InputPair [A, B] => OutputPair [A, B] =
     input =>
      compute_for (input)

  def compute_for (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_with (input.memoized_values.get (input.value) ) (input)

  private def _compute_with (maybe_res : Option [B] ) (input : InputPair [A, B] ) : OutputPair [A, B] =
    if ( maybe_res.isEmpty
    ) compute_and_update (input)
    else OutputPair_ (maybe_res.get, input.memoized_values)

  def compute_and_update (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_and_update_with (input.value) (main_function (input) )

  private def _compute_and_update_with (input_value : A) (output : OutputPair [A, B] ) : OutputPair [A, B] =
    _add_element (output, Tuple2 (input_value, output.value) )

  private def _add_element (output : OutputPair [A, B], new_pair : Tuple2 [A, B] ) : OutputPair [A, B] =
    OutputPair_ (output.value, output.memoized_values + new_pair)

}

case class Memoizer_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends Memoizer [A, B]

trait HardProblem
  extends
    MemoizableFunction [Int, Int]
{

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  def is_even (n : Int) : Boolean =
    n % 2 == 0

  def one_step (n : Int) : Int =
    if ( is_even (n)
    ) n / 2
    else 3 * n + 1

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      if ( input.value == 1
      ) OutputPair_ (0, input.memoized_values)
      else _plus_one (compute (InputPair_ (one_step (input.value), input.memoized_values) ) )

  private def _plus_one (pair : OutputPair [Int, Int] ) : OutputPair [Int, Int] =
    OutputPair_ (1 + pair.value, pair.memoized_values)

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer.compute (input)

}

case class HardProblem_ () extends HardProblem

trait MemoizedFibonacci
  extends
    MemoizableFunction [Int, Int]
{

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      main_function_for (input)

  def main_function_for (input : InputPair [Int, Int] ): OutputPair [Int, Int] =
    if ( (input.value == 0) || (input.value == 1)
    ) OutputPair_ (input.value, input.memoized_values )
    else _compute_and_update_1 (compute (InputPair_ (input.value - 2, input.memoized_values ) ) ) (input.value)

  private def _compute_and_update_1 (first_tuple : OutputPair [Int, Int] ) (n : Int ) : OutputPair [Int, Int] =
    _compute_and_update_2 (first_tuple.value) (compute (InputPair_ (n - 1, first_tuple.memoized_values) ) ) (n)

  private def _compute_and_update_2 (first_value : Int) (second_tuple : OutputPair [Int, Int] ) (n : Int ) : OutputPair [Int, Int] =
    _compute_and_update_3 (_get_next_fibo (first_value) (second_tuple.value) ) (second_tuple.memoized_values) (n)

  private def _compute_and_update_3 (res : Int) (second_map : Map [Int, Int] ) (n : Int) : OutputPair [Int, Int] =
    OutputPair_ (res, second_map + Tuple2 (n, res) )

  private def _get_next_fibo (a : Int) (b : Int) : Int =
    a + b

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer.compute (input)

}

case class MemoizedFibonacci_ () extends MemoizedFibonacci


trait Status
{

  def   r : BigInt
  def   n : Int
  def   q : BigInt
  def   t : BigInt
  def   l : Int
  def   k : Int

  override
  lazy val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k

}

case class Status_ (r : BigInt, n : Int, q : BigInt, t : BigInt, l : Int, k : Int) extends Status

trait PiIterator
{

  def apply (n : Int) : Seq [Int] =
    _tailrec_take (n) (Seq () ) (_initial_status) (_get_next (_initial_status) )

  private lazy val _initial_status =
    Status_ (r = 0, n = 3, q = 1, t = 1, l = 3, k = 1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_compute_new_status (s : Status) : Status =
    if ( (4 * s.q + s.r - s.t) < (s.n * s.t)
    ) s
    else
      _tailrec_compute_new_status (
        Status_ (
          r = (2 * s.q + s.r) * s.l,
          n = ( (s.q * (7 * s.k) + 2 + (s.r * s.l) ) / (s.t * s.l) ).toInt,
          q = s.q * s.k,
          t = s.t * s.l,
          l = s.l + 2,
          k = s.k + 1
        )
      )

  private def _compute_new_status (s : Status) : Status =
    _tailrec_compute_new_status (s)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_take (n : Int) (rev_seq : Seq [Int] ) (s : Status) (t : IntAndStatus) : Seq [Int] =
    if ( n == 0
    ) rev_seq.reverse
    else _tailrec_take (n - 1) (rev_seq.+: (t.digit) ) (t.new_status) (_get_next (t.new_status) )

  private def _get_next (s : Status) : IntAndStatus =
    _get_next_with_new_status (_compute_new_status (s) )

  private def _get_next_with_new_status (s : Status) : IntAndStatus =
    IntAndStatus_ (
      s.n,
      Status_ (
        r = 10 * (s.r - s.n * s.t),
        n = ( ( (10 * (3 * s.q + s.r) ) / s.t) - (10 * s.n) ).toInt,
        q = s.q * 10,
        t = s.t,
        l = s.l,
        k = s.k
      )
    )

}

case class PiIterator_ () extends PiIterator

trait IntAndStatus
{

  def   digit : Int
  def   new_status : Status

}

case class IntAndStatus_ (digit : Int, new_status : Status) extends IntAndStatus

