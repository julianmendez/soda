trait SimpleList [A ]
{



}

case class SimpleList_ [A] () extends SimpleList [A]

object SimpleList {
  def mk [A] : SimpleList [A] =
    SimpleList_ [A] ()
}

trait nil [A ]
  extends
    SimpleList [A]
{



}

case class nil_ [A] () extends nil [A]

object nil {
  def mk [A] : nil [A] =
    nil_ [A] ()
}

trait cons [A ]
  extends
    SimpleList [A]
{

  def   e : A
  def   s : SimpleList [A]

}

case class cons_ [A] (e : A, s : SimpleList [A]) extends cons [A]

object cons {
  def mk [A] (e : A) (s : SimpleList [A]) : cons [A] =
    cons_ [A] (e, s)
}

trait SeqList
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_reverse [A ] (a : SimpleList [A] ) (b : SimpleList [A] ) : SimpleList [A] =
    a match  {
      case cons_ (e , s) => _tailrec_reverse (s) (cons_ (e , b) )
      case _otherwise => b
    }

  def reverse [A ] (s : SimpleList [A] ) : SimpleList [A] =
    _tailrec_reverse [A] (s) (nil_ [A] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_Seq [A ] (a : Seq [A] ) (b : SimpleList [A] ) : SimpleList [A] =
    a match  {
      case (e) :: (s) => _tailrec_from_Seq (s) (cons_ (e , b) )
      case _otherwise => b
    }

  def from_Seq [A ] (a : Seq [A] ) : SimpleList [A] =
    reverse (_tailrec_from_Seq (a) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_to_Seq [A ] (a : SimpleList [A] ) (b : Seq [A] ) : Seq [A] =
    a match  {
      case cons_ (e , s) => _tailrec_to_Seq (s) (b .+: (e) )
      case _otherwise => b
    }

  def to_Seq [A ] (a : SimpleList [A] ) : Seq [A] =
    (_tailrec_to_Seq (a) (Seq [A] () ) ) .reverse

}

case class SeqList_ () extends SeqList

object SeqList {
  def mk : SeqList =
    SeqList_ ()
}
