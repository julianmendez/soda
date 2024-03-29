package soda.example.forcoq.lib

/*
 * This package contains helper classes that could be needed for a translation to Coq.
 */

trait list [A ]
{



}

case class list_ [A] () extends list [A]

object list {
  def mk [A] : list [A] =
    list_ [A] ()
}

trait nil [A ]
  extends
    list [A]
{



}

case class nil_ [A] () extends nil [A]

object nil {
  def mk [A] : nil [A] =
    nil_ [A] ()
}

trait cons [A ]
  extends
    list [A]
{

  def   e : A
  def   s : list [A]

}

case class cons_ [A] (e : A, s : list [A]) extends cons [A]

object cons {
  def mk [A] (e : A) (s : list [A]) : cons [A] =
    cons_ [A] (e, s)
}

trait SeqList
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_reverse [A ] (a : list [A] ) (b : list [A] ) : list [A] =
    a match  {
      case cons_ (e , s) => _tailrec_reverse (s) (cons_ (e , b) )
      case _otherwise => b
    }

  def reverse [A ] (s : list [A] ) : list [A] =
    _tailrec_reverse [A] (s) (nil_ [A] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_Seq [A ] (a : Seq [A] ) (b : list [A] ) : list [A] =
    a match  {
      case Nil => b
      case (e) :: (s) => _tailrec_from_Seq (s) (cons_ (e , b) )
    }

  def from_Seq [A ] (a : Seq [A] ) : list [A] =
    reverse (_tailrec_from_Seq (a) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_to_Seq [A ] (a : list [A] ) (b : Seq [A] ) : Seq [A] =
    a match  {
      case cons_ (e , s) => _tailrec_to_Seq (s) (b .+: (e) )
      case _otherwise => b
    }

  def to_Seq [A ] (a : list [A] ) : Seq [A] =
    (_tailrec_to_Seq (a) (Seq [A] () ) ) .reverse

}

case class SeqList_ () extends SeqList

object SeqList {
  def mk : SeqList =
    SeqList_ ()
}


trait nat
{

  def   add : nat => nat
  def   mul : nat => nat

}

case class nat_ (add : nat => nat, mul : nat => nat) extends nat

object nat {
  def mk (add : nat => nat) (mul : nat => nat) : nat =
    nat_ (add, mul)
}

trait O
  extends
    nat
{



  lazy val add : nat => nat =
     a => add_for (a)

  def add_for (a : nat) : nat =
    a

  lazy val mul : nat =>  nat =
     a => mul_for (a)

  def mul_for (a : nat) : nat =
    this

}

case class O_ () extends O

object O {
  def mk : O =
    O_ ()
}

trait S
  extends
    nat
{

  def   k : nat

  lazy val t = IntNat_ ()

  lazy val add : nat => nat =
     a => add_for (a)

  def add_for (a : nat) : nat =
    t .from_non_negative ( (t .to_Int (k) + 1) + t .to_Int (a) )

  lazy val mul : nat =>  nat =
     a => mul_for (a)

  def mul_for (a : nat) : nat =
    t .from_non_negative ( (t .to_Int (k) + 1) * t .to_Int (a) )

}

case class S_ (k : nat) extends S

object S {
  def mk (k : nat) : S =
    S_ (k)
}

trait IntNat
{



  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_non_negative (a : Int) (b : nat) : nat =
    if ( a <= 0
    ) b
    else _tailrec_from_non_negative (a - 1) (S_ (b) )

  def from_non_negative (a : Int) : nat =
    _tailrec_from_non_negative (a) (O_ () )

  def from_Int (a : Int) : OptionSD [nat] =
    if ( a < 0
    ) NoneSD_ [nat] ()
    else SomeSD_ [nat] (from_non_negative (a) )

  def to_Int (a : nat) : Int =
    a match  {
      case S_ (k) => 1 + to_Int (k)
      case _otherwise => 0
    }

}

case class IntNat_ () extends IntNat

object IntNat {
  def mk : IntNat =
    IntNat_ ()
}

