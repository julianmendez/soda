package soda.example.forcoq.lib


trait nat {

  def add (a: nat ): nat
}

case class O ()  extends nat {

  def add (a: nat ): nat = a
}

case class S (k: nat )  extends nat {

  def add (a: nat ): nat =
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
        else _from_non_negative (a - 1, S (b )  )

       _from_non_negative (a, O ()  ) }

  def to_Int (a: nat ): Int =
    a  match {
      case O () => 0
      case S (k ) => 1 + to_Int (k )
    }
}

case class IntNat_ ()  extends IntNat
