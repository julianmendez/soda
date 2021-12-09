package soda.example.forcoq.mathematics

trait FiboExampleInSodaForCoq {

  import soda.example.forcoq.lib.nat
  import soda.example.forcoq.lib.O
  import soda.example.forcoq.lib.S

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_fib (m: nat ) (a: nat ) (b: nat ): nat =
    m match  {
      case O () => a
      case S (O ()  ) => b
      case S (k ) => _tailrec_fib (k ) (b ) (a .add (b )  )
    }

  def fib (n: nat ) =
    _tailrec_fib (n ) (O ()  ) (S (O () ) )

}

case class FiboExampleInSodaForCoq_ ()  extends FiboExampleInSodaForCoq
