trait FactorialWithFold
{

  lazy val fold = soda.lib.Fold_ ()

  lazy val range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    fold .apply (range .apply (n) ) (1) ( accum =>  k => (accum * (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold
