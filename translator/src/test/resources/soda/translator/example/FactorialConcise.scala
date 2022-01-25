package soda.example

trait AbstractFactorialConcise {

  def   get_factorial (n: Int ): Int

}

trait FactorialConcise
  extends AbstractFactorialConcise {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (n: Int, product: Int ): Int =
    if (n == 0
    ) product
    else _tailrec_get_factorial (n - 1, n * product )

  def get_factorial (n: Int ): Int =
    _tailrec_get_factorial (n, 1 )

}

case class FactorialConcise_ ()
  extends FactorialConcise
