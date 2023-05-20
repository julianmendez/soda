trait FiboExampleInSodaForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fib (m : nat) (a : nat) (b : nat) : nat =
    m match  {
      case S_ (O_ () ) => b
      case S_ (k) => _tailrec_fib (k) (b) (a .add (b) )
      case otherwise => a
    }

  def fib (n : nat) =
    _tailrec_fib (n) (O_ () ) ( S_ (O_ () ) )

}

case class FiboExampleInSodaForCoq_ () extends FiboExampleInSodaForCoq
