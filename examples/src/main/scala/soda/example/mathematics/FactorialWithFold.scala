package soda.example.mathematics

trait FactorialWithFold
{

  import   soda.lib.Fold_
  import   soda.lib.Range_

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  def get_factorial (n : Int) : Int =
    _fold.apply (_range.apply (n) ) (1) (  (product : Int) =>  (k : Int) => (product *  (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold
