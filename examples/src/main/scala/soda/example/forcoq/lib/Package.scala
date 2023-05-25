package soda.example.forcoq.lib

/*
 * This package contains helper classes that could be needed for a translation to Coq.
 */

trait Package

trait list [A]
{

}

case class list_ [A] () extends list [A]

trait nil [A]
  extends
    list [A]
{

}

case class nil_ [A] () extends nil [A]

trait cons [A]
  extends
    list [A]
{

  def   e : A
  def   s : list [A]

}

case class cons_ [A] (e : A, s : list [A]) extends cons [A]

trait SeqList
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_reverse [A] (a : list [A] ) (b : list [A] ) : list [A] =
    a match  {
      case cons_ (e , s) => _tailrec_reverse (s) (cons_ (e , b) )
      case otherwise => b
    }

  def reverse [A] (s : list [A] ) : list [A] =
    _tailrec_reverse [A] (s) (nil_ [A] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_Seq [A] (a : Seq [A] ) (b : list [A] ) : list [A] =
    a match  {
      case (e) :: (s) => _tailrec_from_Seq (s) (cons_ (e , b) )
      case otherwise => b
    }

  def from_Seq [A] (a : Seq [A] ) : list [A] =
    reverse (_tailrec_from_Seq (a) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_to_Seq [A] (a : list [A] ) (b : Seq [A] ) : Seq [A] =
    a match  {
      case cons_ (e , s) => _tailrec_to_Seq (s) (b .+: (e) )
      case otherwise => b
    }

  def to_Seq [A] (a : list [A] ) : Seq [A] =
    (_tailrec_to_Seq (a) (Seq [A] () ) ) .reverse

}

case class SeqList_ () extends SeqList


trait nat
{

  def   add : nat => nat
  def   mul : nat => nat

}

case class nat_ (add : nat => nat, mul : nat => nat) extends nat

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

trait IntNat
{

  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  def from_Int (a : Int) : OptionSD [nat] =
    if ( a < 0
    ) NoneSD_ [nat] ()
    else SomeSD_ [nat] (from_non_negative (a) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_non_negative (a : Int) (b : nat) : nat =
    if ( a <= 0
    ) b
    else _tailrec_from_non_negative (a - 1) (S_ (b) )

  def from_non_negative (a : Int) : nat =
    _tailrec_from_non_negative (a) (O_ () )

  def to_Int (a : nat) : Int =
    a match  {
      case S_ (k) => 1 + to_Int (k)
      case otherwise => 0
    }

}

case class IntNat_ () extends IntNat

