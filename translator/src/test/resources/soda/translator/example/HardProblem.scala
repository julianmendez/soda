package soda.example

trait MemoizableFunction [A, B] {

  def compute (x: A, memoized_values: Map [A, B]  ): Tuple2 [B, Map [A, B]]

}

trait MainFunction [A, B] {

  def main_function: (A, Map [A, B]  ) => Tuple2 [B, Map [A, B]]

}

trait Memoizer [A, B]
  extends MemoizableFunction [A, B] with MainFunction [A, B] {

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

case class Memoizer_ [A, B]  (main_function: (A, Map [A, B]  ) => Tuple2 [B, Map [A, B]]  )
  extends Memoizer [A, B]

trait HardProblem
  extends MemoizableFunction [Int, Int] {

  lazy val memoizer = Memoizer_ [Int, Int]  (main_function )

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

  def compute (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    memoizer.compute (n, memoized_values )

}

case class HardProblem_ ()
  extends HardProblem

trait MemoizedFibonacci
  extends MemoizableFunction [Int, Int] {

  lazy val memoizer = Memoizer_ [Int, Int]  (main_function )

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

  def compute (n: Int, memoized_values: Map [Int, Int]  ): Tuple2 [Int, Map [Int, Int]] =
    memoizer.compute (n, memoized_values )

}

case class MemoizedFibonacci_ ()
  extends MemoizedFibonacci
