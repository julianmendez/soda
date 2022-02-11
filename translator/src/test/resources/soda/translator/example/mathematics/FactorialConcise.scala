package soda.example.mathematics

trait AbstractFactorialConcise
{

  def   get_factorial: Int => Int

}

case class AbstractFactorialConcise_ (get_factorial: Int => Int) extends AbstractFactorialConcise

trait FactorialConcise
  extends
    AbstractFactorialConcise
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (n: Int ) (product: Int ): Int =
    if (n == 0
    ) product
    else _tailrec_get_factorial (n - 1 ) (n * product )

  lazy val get_factorial: Int => Int =
     n => get_factorial_for (n )

  def get_factorial_for (n: Int ): Int =
    _tailrec_get_factorial (n ) (1 )

}

case class FactorialConcise_ () extends FactorialConcise
