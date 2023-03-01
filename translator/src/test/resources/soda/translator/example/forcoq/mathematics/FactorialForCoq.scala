trait FactorialForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (m : nat) (product : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_factorial (k) (product .mul ( S_ (k) ) )
      case x => product
    }

  def get_factorial (n : nat) : nat =
    _tailrec_get_factorial (n) (S_ ( O_ () ) )

}

case class FactorialForCoq_ () extends FactorialForCoq
