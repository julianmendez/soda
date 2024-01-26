trait FactorialSimple
{

  def apply (n : Int) : Int =
    if ( n < 2
    ) 1
    else n * apply (n - 1)

}

case class FactorialSimple_ () extends FactorialSimple

object FactorialSimple {
  def mk : FactorialSimple =
    FactorialSimple_ ()
}
