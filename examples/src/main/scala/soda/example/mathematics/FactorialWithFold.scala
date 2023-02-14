package soda.example.mathematics

/*
 * This package contains examples in Soda.
 * These examples use mathematical properties.
 */



trait FactorialWithFold
{

  private lazy val _fold = soda.lib.Fold_ ()

  private lazy val _range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    _fold.apply (_range.apply (n) ) (1) (  product =>  k => (product *  (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold
