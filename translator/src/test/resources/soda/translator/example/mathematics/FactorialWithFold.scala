trait FactorialWithFold
{

  private lazy val _fold = soda.lib.Fold_ ()

  private lazy val _range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    if ( n < 0
    ) 0
    else _fold .apply (_range .apply (n) ) (1) ( accum =>  k => (accum * (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold
