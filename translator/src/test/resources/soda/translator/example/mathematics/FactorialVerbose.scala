package soda.example.mathematics

trait AbstractFactorialVerbose
{

  def   get_factorial : Int => Int

}

case class AbstractFactorialVerbose_ (get_factorial : Int => Int) extends AbstractFactorialVerbose

trait FactorialVerbose
  extends
    AbstractFactorialVerbose
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    if ( n == 0
    ) product
    else _tailrec_get_factorial (n - 1) (n * product)

  lazy val get_factorial =
     n => get_factorial_for (n)

  def get_factorial_for (n : Int) =
    _tailrec_get_factorial (n) (1)

}

case class FactorialVerbose_ () extends FactorialVerbose
