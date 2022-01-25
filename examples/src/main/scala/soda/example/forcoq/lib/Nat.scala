package soda.example.forcoq.lib

trait nat {

  def   add (a: nat ): nat
  def   mul (a: nat ): nat

}

case class O ()
  extends nat {

  def add (a: nat ): nat = a

  def mul (a: nat ): nat = this

}

case class S (k: nat )
  extends nat {

  lazy val t = IntNat_ ()

  def add (a: nat ): nat =
    t.from_non_negative ((t.to_Int (k ) + 1 ) + t.to_Int (a ) )

  def mul (a: nat ): nat =
    t.from_non_negative ((t.to_Int (k ) + 1 ) * t.to_Int (a ) )

}

trait IntNat {

  import   soda.lib.OptionSD
  import   soda.lib.NoneSD_
  import   soda.lib.SomeSD_

  def from_Int (a: Int ): OptionSD [nat] =
    if (a < 0
    ) NoneSD_ [nat] ()
    else SomeSD_ [nat] (from_non_negative (a )  )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_from_non_negative (a: Int, b: nat ): nat =
    if (a <= 0
    ) b
    else _tailrec_from_non_negative (a - 1, S (b )  )

  def from_non_negative (a: Int ): nat =
    _tailrec_from_non_negative (a, O ()  )

  def to_Int (a: nat ): Int =
    a match  {
      case O () => 0
      case S (k ) => 1 + to_Int (k )
    }

}

case class IntNat_ ()
  extends IntNat
