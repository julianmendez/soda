trait TriangularNumberForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_number (m : nat) (acc : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      case otherwise => acc
    }

  def get_number (n : nat) : nat =
    _tailrec_get_number (n) ( O_ () )

}

case class TriangularNumberForCoq_ () extends TriangularNumberForCoq
