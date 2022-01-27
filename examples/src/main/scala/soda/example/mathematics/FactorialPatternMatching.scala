package soda.example.mathematics

trait FactorialPatternMatching
  extends
    AbstractFactorialConcise
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (n: Int, product: Int ): Int =
    n match  {
      case 0 => product
      case k => _tailrec_get_factorial (k - 1, k * product )
    }

  def get_factorial (n: Int ): Int =
    _tailrec_get_factorial (n, 1 )

}

case class FactorialPatternMatching_ ()
  extends
    FactorialPatternMatching
{

}
