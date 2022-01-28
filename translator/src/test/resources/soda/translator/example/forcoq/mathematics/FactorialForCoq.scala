package soda.example.forcoq.mathematics

trait FactorialForCoq
{

  import   soda.example.forcoq.lib.nat
  import   soda.example.forcoq.lib.O
  import   soda.example.forcoq.lib.S

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_factorial (m: nat ) (product: nat ): nat =
    m match  {
      case O () => product
      case S (k ) => _tailrec_get_factorial (k ) (product .mul (S (k ) )  )
    }

  def get_factorial (n: nat ): nat =
    _tailrec_get_factorial (n ) (S (O () ) )

}

case class FactorialForCoq_ ()
  extends
    FactorialForCoq
{

}
