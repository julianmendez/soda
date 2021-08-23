package soda.translator.example


trait Memoizer [A, B] {
  import soda.lib.OptionSD
  import soda.lib.SomeSD
  import soda.lib.NoneSD

  def memoized_values: Map [A, B]

  def main_function: A => B

  def computation_condition: A => Boolean

  def compute (x: A ): Memoizer [A, B] =
    if (computation_condition (x )
    )
      {
        lazy val new_pair = (x, _get (x )  )
        _Memoizer_ (memoized_values + new_pair, main_function, computation_condition ) }
    else this

  def get (x: A ): Option [B] =
    if (computation_condition (x )
    ) Some (_get (x )  )
    else None

  def _get (x: A ): B =
    {
      lazy val maybe_value = memoized_values.get (x )
      lazy val result =
        if (maybe_value.isEmpty
        ) main_function (x )
        else maybe_value.get
      result }
}

case class _Memoizer_ [A, B]  (memoized_values: Map [A, B], main_function: A => B, computation_condition: A => Boolean )  extends Memoizer [A, B]

trait HardProblem  extends Memoizer [Int, Int] {

  def is_even (n: Int ): Boolean =
    n % 2 == 0

  def one_step (n: Int ): Int =
    if (is_even (n )
    ) n / 2
    else 3 * n + 1

  lazy val main_function: Int => Int = (n: Int ) =>
    if (n == 1
    ) 2
    else get (one_step (n )  ) .get + 1

  lazy val computation_condition: Int => Boolean = (n: Int ) =>
    n > 0
}

case class _HardProblem_ (memoized_values: Map [Int, Int]  )  extends HardProblem

trait HardProblemBuilder {

  lazy val memoized_values = Map [Int, Int]  ()

  lazy val build =
    _HardProblem_ (memoized_values )
}

case class HardProblemBuilder_ () extends HardProblemBuilder
