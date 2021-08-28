package soda.translator.example


trait FactorialWithFold {
  import soda.lib.Recursion_

  def get_factorial (n: Int ): Int =
    Recursion_ () .fold (Recursion_ () .range (n ), 1, (product, k ) => product * (k + 1 ) )
}

case class FactorialWithFold_ () extends FactorialWithFold
