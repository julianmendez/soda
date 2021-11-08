package soda.example.forcoq


trait TriangularNumberForCoq {
  import soda.example.forcoq.lib.{ nat, O, S }

  def rec (m: nat ) (acc: nat ): nat =
    m  match {
      case O () => acc
      case S (k ) => rec (k ) (acc .add (S (k ) )  )
    } ;

  def get_number (n: nat ): nat =
    rec (n ) (O () ) ;
}

case class TriangularNumberForCoq_ ()  extends TriangularNumberForCoq
