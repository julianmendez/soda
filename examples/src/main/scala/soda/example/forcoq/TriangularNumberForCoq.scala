package soda.example.forcoq


trait TriangularNumberForCoq {
  import soda.example.forcoq.lib.{ nat, O, S }

  def _rec_get_number (m: nat ) (acc: nat ): nat =
    m  match {
      case O () => acc
      case S (k ) => _rec_get_number (k ) (acc .add (S (k ) )  )
    } ;

  def get_number (n: nat ): nat =
    _rec_get_number (n ) (O () ) ;
}

case class TriangularNumberForCoq_ ()  extends TriangularNumberForCoq
