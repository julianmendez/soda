package soda.example.forcoq

trait FactorialForCoq =
{

  import   soda.example.forcoq.lib.nat
  import   soda.example.forcoq.lib.O
  import   soda.example.forcoq.lib.S

  def _rec_get_factorial (m: nat ) (product: nat ): nat =
    m match  {
      case O () => product
      case S (k ) => _rec_get_factorial (k ) (product .mul (S (k ) )  )
    }

  def get_factorial (n: nat ): nat =
    _rec_get_factorial (n ) (S (O () ) )

}

case class FactorialForCoq_ ()
  extends FactorialForCoq
{
