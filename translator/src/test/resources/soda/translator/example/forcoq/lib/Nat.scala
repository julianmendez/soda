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
