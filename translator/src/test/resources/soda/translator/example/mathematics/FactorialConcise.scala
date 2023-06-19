trait FactorialConcise
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fact (n : Int) (accum : Int) : Int =
    if ( n < 2
    ) accum
    else _tailrec_fact (n - 1) (n * accum)

  def apply (n : Int) : Int =
    _tailrec_fact (n) (1)

}

case class FactorialConcise_ () extends FactorialConcise
