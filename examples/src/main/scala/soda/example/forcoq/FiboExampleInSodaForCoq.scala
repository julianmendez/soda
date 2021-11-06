package soda.example.forcoq


trait FiboExampleInSodaForCoq {

  def rec_fib (m: nat ) (a: nat ) (b: nat ): nat =
    m  match {
      case nat_O () => a
      case nat_S (nat_O ()  ) => b
      case nat_S (k ) => rec_fib (k ) (b ) (a.plus (b )  )
    }

  def fib (n: nat ) =
    rec_fib (n ) (nat_O ()  ) (nat_S (nat_O ()  )  )
}

case class FiboExampleInSodaForCoq_ ()  extends FiboExampleInSodaForCoq
