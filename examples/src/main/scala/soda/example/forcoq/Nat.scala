package soda.example.forcoq


trait nat {

  def plus (a: nat ): nat
}

case class nat_O ()  extends nat {

  def plus (a: nat ): nat = a
}

case class nat_S (k: nat )  extends nat {

  def plus (a: nat ): nat =
    {
      lazy val m = IntNat_ ()
      m.from_non_negative ((m.to_Int (k ) + 1 ) + m.to_Int (a ) ) }
}

trait IntNat {
  import soda.lib.OptionSD
  import soda.lib.NoneSD_
  import soda.lib.SomeSD_
  import soda.lib.Recursion_

  def from_Int (a: Int ): OptionSD [nat] =
    if (a < 0
    ) NoneSD_ [nat] ()
    else SomeSD_ [nat] (from_non_negative (a )  )

  def from_non_negative (a: Int ): nat =
    {
      import scala.annotation.tailrec
        @tailrec
      def _from_non_negative (a: Int, b: nat ): nat =
        if (a <= 0
        ) b
        else _from_non_negative (a - 1, nat_S (b )  )

       _from_non_negative (a, nat_O ()  ) }

  def to_Int (a: nat ): Int =
    a  match {
      case nat_O () => 0
      case nat_S (k ) => 1 + to_Int (k )
    }
}

case class IntNat_ ()  extends IntNat
