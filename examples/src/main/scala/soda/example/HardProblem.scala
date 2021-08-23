package soda.translator.example

trait Memoizer [A, B] {

  def main_function (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]]

  def compute (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    {
      lazy val maybe_res = memoized_values.get (x )
      lazy val result =
        if (maybe_res.isEmpty
        ) compute_and_update (x, memoized_values )
        else (maybe_res.get, memoized_values )
      result }

  def compute_and_update (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]] =
    {
      lazy val (res, map ) = main_function (x, memoized_values )
      lazy val new_pair = (x, res )
      (res, map + new_pair ) }
}

trait HardProblem  extends Memoizer [Int, Int] {

  def is_even (n: Int ): Boolean =
    n % 2 == 0

  def one_step (n: Int ): Int =
    if (is_even (n )
    ) n / 2
    else 3 * n + 1

  def main_function (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    if (n == 1
    ) (0, memoized_values )
    else
      {
        lazy val (res, new_map ) = compute (one_step (n ), memoized_values )
        (1 + res, new_map ) }
}

case class HardProblem_ () extends HardProblem

trait MemoizedFibonacci  extends Memoizer [Int, Int] {

  def main_function (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    if (n == 0 ) (0, memoized_values )
    else if (n == 1 ) (1, memoized_values )
    else
      {
        lazy val (res1, map1 ) = compute (n - 2, memoized_values )
        lazy val (res2, map2 ) = compute (n - 1, map1 )
        lazy val res = res1 + res2
        lazy val new_pair = (n, res )
        (res, map2 + new_pair ) }
}

case class MemoizedFibonacci_ () extends MemoizedFibonacci
