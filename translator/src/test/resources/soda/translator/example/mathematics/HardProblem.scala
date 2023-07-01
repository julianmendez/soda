trait InputPair [A , B ]
{

  def   value : A
  def   memoized_values : Map [A, B]

}

case class InputPair_ [A, B] (value : A, memoized_values : Map [A, B]) extends InputPair [A, B]

trait OutputPair [A , B ]
{

  def   value : B
  def   memoized_values : Map [A, B]

}

case class OutputPair_ [A, B] (value : B, memoized_values : Map [A, B]) extends OutputPair [A, B]

trait MemoizableFunction [A , B ]
{

    /** compute (input : InputPair [A] [B] ) : OutputPair [A] [B] */
  def   abs_compute : InputPair [A, B] => OutputPair [A, B]

  def compute (input : InputPair [A, B] ) : OutputPair [A, B] =
    abs_compute (input)

  def mk_InputPair (value : A) (memoized_values : Map [A, B] ) : InputPair [A, B] =
    InputPair_ (value, memoized_values)

  def mk_OutputPair (value : B) (memoized_values : Map [A, B] ) : OutputPair [A, B] =
    OutputPair_ (value, memoized_values)

}

case class MemoizableFunction_ [A, B] (abs_compute : InputPair [A, B] => OutputPair [A, B]) extends MemoizableFunction [A, B]

trait MainFunction [A , B ]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

}

case class MainFunction_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends MainFunction [A, B]

trait Memoizer [A , B ]
  extends
    MemoizableFunction [A, B]
    with MainFunction [A, B]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

  private def _add_element (output : OutputPair [A, B] ) (new_pair : Tuple2 [A, B] )
      : OutputPair [A, B] =
    mk_OutputPair (output .value) (output .memoized_values + new_pair)

  private def _compute_and_update_with (input_value : A) (output : OutputPair [A, B] )
      : OutputPair [A, B] =
    _add_element (output) (Tuple2 (input_value, output .value) )

  def compute_and_update (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_and_update_with (input .value) (main_function (input) )

  private def _compute_with (maybe_res : Option [B] ) (input : InputPair [A, B] ) : OutputPair [A, B] =
    if ( maybe_res .isEmpty
    ) compute_and_update (input)
    else mk_OutputPair (maybe_res .get) (input .memoized_values)

  def compute_for (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_with (input .memoized_values .get (input .value) ) (input)

  lazy val abs_compute : InputPair [A, B] => OutputPair [A, B] =
     input =>
      compute_for (input)

}

case class Memoizer_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends Memoizer [A, B]

trait HardProblem
  extends
    MemoizableFunction [Int, Int]
{

  def is_even (n : Int) : Boolean =
    n % 2 == 0

  def one_step (n : Int) : Int =
    if ( is_even (n)
    ) n / 2
    else 3 * n + 1

  private def _plus_one (pair : OutputPair [Int, Int] ) : OutputPair [Int, Int] =
    mk_OutputPair (1 + pair .value) (pair .memoized_values)

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      if ( input .value == 1
      ) mk_OutputPair (0) (input .memoized_values)
      else _plus_one (compute (
        mk_InputPair (one_step (input .value) ) (input .memoized_values) ) )

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer .compute (input)

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

}

case class HardProblem_ () extends HardProblem

trait MemoizedFibonacci
  extends
    MemoizableFunction [Int, Int]
{

  private def _get_next_fibo (a : Int) (b : Int) : Int =
    a + b

  private def _compute_and_update_3 (res : Int) (second_map : Map [Int, Int] ) (n : Int)
      : OutputPair [Int, Int] =
    mk_OutputPair (res) (second_map + Tuple2 (n, res) )

  private def _compute_and_update_2 (first_value : Int) (second_tuple : OutputPair [Int, Int] ) (n : Int )
      : OutputPair [Int, Int] =
    _compute_and_update_3 (_get_next_fibo (first_value) (second_tuple .value) ) (
      second_tuple .memoized_values) (n)

  private def _compute_and_update_1 (first_tuple : OutputPair [Int, Int] ) (n : Int )
      : OutputPair [Int, Int] =
    _compute_and_update_2 (first_tuple .value) (compute (
      mk_InputPair (n - 1) (first_tuple .memoized_values) ) ) (n)

  def main_function_for (input : InputPair [Int, Int] ): OutputPair [Int, Int] =
    if ( (input .value == 0) || (input .value == 1)
    ) mk_OutputPair (input .value) (input .memoized_values )
    else _compute_and_update_1 (compute (
      mk_InputPair (input .value - 2) (input .memoized_values ) ) ) (input .value)

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      main_function_for (input)

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer .compute (input)

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

}

case class MemoizedFibonacci_ () extends MemoizedFibonacci
