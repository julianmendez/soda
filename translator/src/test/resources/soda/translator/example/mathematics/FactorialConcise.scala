trait FactorialConcise
{

  def apply (n : Int) : Int =
    if ( n < 0
    ) 0
    else _tailrec_get_factorial (n) (1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    if ( n == 0
    ) product
    else _tailrec_get_factorial (n - 1) (n * product)

}

case class FactorialConcise_ () extends FactorialConcise
