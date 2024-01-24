trait FactorialPatternMatching
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fact (n : Int) (accum : Int) : Int =
    n match  {
      case 0 => accum
      case otherwise => _tailrec_fact (n - 1) (n * accum)
    }

  def apply (n : Int) : Int =
    if ( n < 0
    ) 1
    else _tailrec_fact (n) (1)

}

case class FactorialPatternMatching_ () extends FactorialPatternMatching

object FactorialPatternMatching { def mk   : FactorialPatternMatching  = FactorialPatternMatching_  () }
