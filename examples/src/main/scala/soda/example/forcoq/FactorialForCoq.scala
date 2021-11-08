package soda.example.forcoq


trait FactorialForCoq {
  import soda.example.forcoq.lib.{ nat, O, S }

  def rec (m: nat ) (product: nat ): nat =
    m  match {
      case O () => product
      case S (k ) => rec (k ) (product .mul (S (k ) )  )
    } ;

  def get_factorial (n: nat ): nat =
    rec (n ) (S (O () ) ) ;
}

case class FactorialForCoq_ ()  extends FactorialForCoq
