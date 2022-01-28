package soda.example.mathematics

trait InputPair [A, B]
{

  def   value: A
  def   memoized_values: Map [A, B]

}

case class InputPair_ [A, B] (value: A, memoized_values: Map [A, B]  )
  extends
    InputPair [A, B]
{

}

trait OutputPair [A, B]
{

  def   value: B
  def   memoized_values: Map [A, B]

}

case class OutputPair_ [A, B] (value: B, memoized_values: Map [A, B]  )
  extends
    OutputPair [A, B]
{

}

trait MemoizableFunction [A, B]
{

    /** compute (input: InputPair [A, B] ): OutputPair [A, B] */
  def   abs_compute: InputPair [A, B] => OutputPair [A, B]

  def compute (input: InputPair [A, B] ): OutputPair [A, B] =
    abs_compute (input )

}

trait MainFunction [A, B]
{

  def   main_function: InputPair [A, B] => OutputPair [A, B]

}

trait Memoizer [A, B]
  extends
    MemoizableFunction [A, B]
    with MainFunction [A, B]
{

  lazy val abs_compute: InputPair [A, B] => OutputPair [A, B] =
     input =>
      compute_for (input )

  def compute_for (input: InputPair [A, B] ): OutputPair [A, B] =
    _compute_with (input.memoized_values.get (input.value ), input )

  def _compute_with (maybe_res: Option [B], input: InputPair [A, B] ): OutputPair [A, B] =
    if (maybe_res.isEmpty
    ) compute_and_update (input )
    else OutputPair_ (maybe_res.get, input.memoized_values )

  def compute_and_update (input: InputPair [A, B] ): OutputPair [A, B] =
    _compute_and_update_with (input.value, main_function (input )  )

  def _compute_and_update_with (input_value: A, output: OutputPair [A, B] ): OutputPair [A, B] =
    _add_element (output, Tuple2 (input_value, output.value )  )

  def _add_element (output: OutputPair [A, B], new_pair: Tuple2 [A, B] ): OutputPair [A, B] =
    OutputPair_ (output.value, output.memoized_values + new_pair )

}

case class Memoizer_ [A, B] (main_function: InputPair [A, B] => OutputPair [A, B] )
  extends
    Memoizer [A, B]
{

}

trait HardProblem
  extends
    MemoizableFunction [Int, Int]
{

  lazy val memoizer = Memoizer_ [Int, Int] (main_function )

  def is_even (n: Int ): Boolean =
    n % 2 == 0

  def one_step (n: Int ): Int =
    if (is_even (n )
    ) n / 2
    else 3 * n + 1

  lazy val main_function: InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      if (input.value == 1
      ) OutputPair_ (0, input.memoized_values )
      else _plus_one (compute (InputPair_ (one_step (input.value ), input.memoized_values ) ) )

  def _plus_one (pair: OutputPair [Int, Int] ): OutputPair [Int, Int] =
    OutputPair_ (1 + pair.value, pair.memoized_values )

  lazy val abs_compute: InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input )

  def compute_for (input: InputPair [Int, Int] ): OutputPair [Int, Int] =
    memoizer.compute (input )

}

case class HardProblem_ ()
  extends
    HardProblem
{

}

trait MemoizedFibonacci
  extends
    MemoizableFunction [Int, Int]
{

  lazy val memoizer = Memoizer_ [Int, Int] (main_function )

  lazy val main_function: InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      if (input.value == 0 ) OutputPair_ (0, input.memoized_values )
      else if (input.value == 1 ) OutputPair_ (1, input.memoized_values )
      else _compute_and_update_1 (compute (InputPair_ (input.value - 2, input.memoized_values ) ), input.value )

  def _compute_and_update_1 (first_tuple: OutputPair [Int, Int], n: Int ): OutputPair [Int, Int] =
    _compute_and_update_2 (first_tuple.value, compute (InputPair_ (n - 1, first_tuple.memoized_values ) ), n )

  def _compute_and_update_2 (first_value: Int, second_tuple: OutputPair [Int, Int], n: Int ): OutputPair [Int, Int] =
    _compute_and_update_3 (_get_next_fibo (first_value, second_tuple.value ), second_tuple.memoized_values, n )

  def _compute_and_update_3 (res: Int, second_map: Map [Int, Int], n: Int ): OutputPair [Int, Int] =
    OutputPair_ (res, second_map + Tuple2 (n, res ) )

  def _get_next_fibo (a: Int, b: Int ): Int =
    a + b

  lazy val abs_compute: InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input )

  def compute_for (input: InputPair [Int, Int] ): OutputPair [Int, Int] =
    memoizer.compute (input )

}

case class MemoizedFibonacci_ ()
  extends
    MemoizedFibonacci
{

}
