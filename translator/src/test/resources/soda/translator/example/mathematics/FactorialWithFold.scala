package soda.example.mathematics

trait FactorialWithFold
{

  private lazy val _fold = soda.lib.Fold_ ()

  private lazy val _range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    _fold.apply (_range.apply (n) ) (1) (  product =>  k => (product *  (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold
