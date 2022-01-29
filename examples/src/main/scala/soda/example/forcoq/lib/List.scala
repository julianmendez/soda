package soda.example.forcoq.lib

trait list [A]
{

}

trait nil [A]
  extends
    list [A]
{

}

case class nil_ [A] ()
  extends
    nil [A]
{

}

trait cons [A]
  extends
    list [A]
{

  def   e: A
  def   s: list [A]

}

case class cons_ [A] (e: A, s: list [A]  )
  extends
    cons [A]
{

}

trait SeqList
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_reverse [A] (a: list [A]  ) (b: list [A]  ): list [A] =
    a match  {
      case nil_ () => b
      case cons_ (e, s ) => _tailrec_reverse (s ) (cons_ (e, b ) )
    }

  def reverse [A] (s: list [A]  ): list [A] =
    _tailrec_reverse [A] (s ) (nil_ [A] ()  )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_from_Seq [A] (a: Seq [A]  ) (b: list [A]  ): list [A] =
    a match  {
      case Nil => b
      case (e ):: (s ) => _tailrec_from_Seq (s ) (cons_ (e, b )  )
    }

  def from_Seq [A] (a: Seq [A]  ): list [A] =
    reverse (_tailrec_from_Seq (a ) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_to_Seq [A] (a: list [A]  ) (b: Seq [A]  ): Seq [A] =
    a match  {
      case nil_ () => b
      case cons_ (e, s ) => _tailrec_to_Seq (s ) (b .+: (e ) )
    }

  def to_Seq [A] (a: list [A]  ): Seq [A] =
    (_tailrec_to_Seq (a ) (Seq [A]  () )  ) .reverse

}

case class SeqList_ ()
  extends
    SeqList
{

}
