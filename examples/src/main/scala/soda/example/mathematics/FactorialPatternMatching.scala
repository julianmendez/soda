package soda.example.mathematics

trait FactorialPatternMatching
  extends
    AbstractFactorialConcise
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    n match  {
      case 0 => product
      case k => _tailrec_get_factorial (k - 1) (k * product)
    }

  lazy val get_factorial : Int => Int =
     n => get_factorial_for (n)

  def get_factorial_for (n : Int) : Int =
    _tailrec_get_factorial (n) (1)

}

case class FactorialPatternMatching_ () extends FactorialPatternMatching
