package soda.example.forcoq.mathematics

trait TriangularNumberForCoq
{

  import   soda.example.forcoq.lib.nat
  import   soda.example.forcoq.lib.O
  import   soda.example.forcoq.lib.S

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_get_number (m: nat ) (acc: nat ): nat =
    m match  {
      case O () => acc
      case S (k ) => _tailrec_get_number (k ) (acc .add (S (k ) )  )
    }

  def get_number (n: nat ): nat =
    _tailrec_get_number (n ) (O () )

}

case class TriangularNumberForCoq_ ()
  extends
    TriangularNumberForCoq
{

}
