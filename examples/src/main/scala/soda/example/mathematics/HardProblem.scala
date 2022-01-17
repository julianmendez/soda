package soda.example.mathematics

trait MemoizableFunction [A, B] {

  def compute (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]]

}

trait MainFunction [A, B] {

  def main_function: (A, Map [A, B]  ) => Tuple2 [B, Map [A, B]]

}

trait Memoizer [A, B]
  extends MemoizableFunction [A, B] with MainFunction [A, B] {

  def compute (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    _compute_with (memoized_values.get (x ), x, memoized_values )

  def _compute_with (maybe_res: Option [B], x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    if (maybe_res.isEmpty
    ) compute_and_update (x, memoized_values )
    else Tuple2 (maybe_res.get, memoized_values )

  def compute_and_update (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    _compute_and_update_with (main_function (x, memoized_values ), x, memoized_values )

  def _compute_and_update_with (tuple: Tuple2 [B, Map [A, B]], x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    _add_element (Tuple2 (x, tuple._1 ), tuple, x, memoized_values )

  def _add_element (new_pair: Tuple2 [A, B], tuple: Tuple2 [B, Map [A, B]], x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    Tuple2 (tuple._1, tuple._2 + new_pair )

}

case class Memoizer_ [A, B] (main_function: (A, Map [A, B]  ) => Tuple2 [B, Map [A, B]]  )
  extends Memoizer [A, B]

trait HardProblem
  extends MemoizableFunction [Int, Int] {

  lazy val memoizer = Memoizer_ [Int, Int] (main_function )

  def is_even (n: Int ): Boolean =
    n % 2 == 0

  def one_step (n: Int ): Int =
    if (is_even (n )
    ) n / 2
    else 3 * n + 1

  def main_function (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    if (n == 1
    ) Tuple2 (0, memoized_values )
    else _plus_one (compute (one_step (n ), memoized_values ) )

  def _plus_one (pair: Tuple2 [Int, Map [Int, Int]]  ): Tuple2 [Int, Map [Int, Int]] =
    Tuple2 (1 + pair._1, pair._2 )

  def compute (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    memoizer.compute (n, memoized_values )

}

case class HardProblem_ ()
  extends HardProblem

trait MemoizedFibonacci
  extends MemoizableFunction [Int, Int] {

  lazy val memoizer = Memoizer_ [Int, Int] (main_function )

  def main_function (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    if (n == 0 ) Tuple2 (0, memoized_values )
    else if (n == 1 ) Tuple2 (1, memoized_values )
    else _compute_and_update_1 (compute (n - 2, memoized_values ), n )

  def _compute_and_update_1 (first_tuple: Tuple2 [Int, Map [Int, Int]], n: Int ): Tuple2 [Int, Map [Int, Int]] =
    _compute_and_update_2 (first_tuple._1, compute (n - 1, first_tuple._2 ), n )

  def _compute_and_update_2 (first_value: Int, second_tuple: Tuple2 [Int, Map [Int, Int]], n: Int ): Tuple2 [Int, Map [Int, Int]] =
    _compute_and_update_3 (_get_next_fibo (first_value, second_tuple._1 ), second_tuple._2, n )

  def _compute_and_update_3 (res: Int, second_map: Map [Int, Int], n: Int ): Tuple2 [Int, Map [Int, Int]] =
    Tuple2 (res, second_map + Tuple2 (n, res ) )

  def _get_next_fibo (a: Int, b: Int ): Int =
    a + b

  def compute (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    memoizer.compute (n, memoized_values )

}

case class MemoizedFibonacci_ ()
  extends MemoizedFibonacci
