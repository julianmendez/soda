package soda.example.forcoq


trait FiboExampleInSodaForCoq {
  import soda.example.forcoq.lib.{ nat, O, S }

  def _rec_fib (m: nat ) (a: nat ) (b: nat ): nat =
    m  match {
      case O () => a
      case S (O ()  ) => b
      case S (k ) => _rec_fib (k ) (b ) (a .add (b )  )
    } ;

  def fib (n: nat ) =
    _rec_fib (n ) (O ()  ) (S (O () ) ) ;
}

case class FiboExampleInSodaForCoq_ ()  extends FiboExampleInSodaForCoq
