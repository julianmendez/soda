package soda.example.forlean.lib

/*
 * This package contains helper classes that could be needed for a translation to Coq.
 */

trait Package

trait List [A ]
{

}

case class List_ [A] () extends List [A]

trait nil [A ]
  extends
    List [A]
{

}

case class nil_ [A] () extends nil [A]

trait cons [A ]
  extends
    List [A]
{

  def   e : A
  def   s : List [A]

}

case class cons_ [A] (e : A, s : List [A]) extends cons [A]

trait SeqList
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_reverse [A ] (a : List [A] ) (b : List [A] ) : List [A] =
    a match  {
      case cons_ (e , s) => _tailrec_reverse (s) (cons_ (e , b) )
      case otherwise => b
    }

  def reverse [A ] (s : List [A] ) : List [A] =
    _tailrec_reverse [A] (s) (nil_ [A] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_Seq [A ] (a : Seq [A] ) (b : List [A] ) : List [A] =
    a match  {
      case (e) :: (s) => _tailrec_from_Seq (s) (cons_ (e , b) )
      case otherwise => b
    }

  def from_Seq [A ] (a : Seq [A] ) : List [A] =
    reverse (_tailrec_from_Seq (a) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_to_Seq [A ] (a : List [A] ) (b : Seq [A] ) : Seq [A] =
    a match  {
      case cons_ (e , s) => _tailrec_to_Seq (s) (b .+: (e) )
      case otherwise => b
    }

  def to_Seq [A ] (a : List [A] ) : Seq [A] =
    (_tailrec_to_Seq (a) (Seq [A] () ) ) .reverse

}

case class SeqList_ () extends SeqList


trait Nat
{

  def   add : Nat => Nat
  def   mul : Nat => Nat

}

case class Nat_ (add : Nat => Nat, mul : Nat => Nat) extends Nat

trait O
  extends
    Nat
{

  lazy val add : Nat => Nat =
     a => add_for (a)

  def add_for (a : Nat) : Nat =
    a

  lazy val mul : Nat =>  Nat =
     a => mul_for (a)

  def mul_for (a : Nat) : Nat =
    this

}

case class O_ () extends O

trait S
  extends
    Nat
{

  def   k : Nat

  lazy val t = IntNat_ ()

  lazy val add : Nat => Nat =
     a => add_for (a)

  def add_for (a : Nat) : Nat =
    t .from_non_negative ( (t .to_Int (k) + 1) + t .to_Int (a) )

  lazy val mul : Nat =>  Nat =
     a => mul_for (a)

  def mul_for (a : Nat) : Nat =
    t .from_non_negative ( (t .to_Int (k) + 1) * t .to_Int (a) )

}

case class S_ (k : Nat) extends S

trait IntNat
{

  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_non_negative (a : Int) (b : Nat) : Nat =
    if ( a <= 0
    ) b
    else _tailrec_from_non_negative (a - 1) (S_ (b) )

  def from_non_negative (a : Int) : Nat =
    _tailrec_from_non_negative (a) (O_ () )

  def from_Int (a : Int) : OptionSD [Nat] =
    if ( a < 0
    ) NoneSD_ [Nat] ()
    else SomeSD_ [Nat] (from_non_negative (a) )

  def to_Int (a : Nat) : Int =
    a match  {
      case S_ (k) => 1 + to_Int (k)
      case otherwise => 0
    }

}

case class IntNat_ () extends IntNat

