trait FactorialPatternMatching
{

  def apply (n : Int) : Int =
    _tailrec_get_factorial (n) (1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    n match  {
      case 0 => product
      case x => _tailrec_get_factorial (x - 1) (x * product)
    }

}

case class FactorialPatternMatching_ () extends FactorialPatternMatching
