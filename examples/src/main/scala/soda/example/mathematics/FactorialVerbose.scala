package soda.example.mathematics

trait AbstractFactorialVerbose {

  def get_factorial (n: Int ): Int

}

trait FactorialVerbose  extends AbstractFactorialVerbose {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (n: Int, product: Int ): Int =
    if (n == 0
    ) product
    else _tailrec_get_factorial (n - 1, n * product )

  def get_factorial (n: Int ) =
    _tailrec_get_factorial (n, 1 )

}

case class FactorialVerbose_ ()  extends FactorialVerbose
