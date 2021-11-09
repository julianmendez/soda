package soda.example.forcoq.lib


trait list [A]

case class nil [A] ()  extends list [A]

case class cons [A] (e: A, s: list [A]  )  extends list [A]

trait SeqList {
  import soda.lib.OptionSD
  import soda.lib.NoneSD_
  import soda.lib.SomeSD_
  import soda.lib.Recursion_


  def _rec_reverse [A] (a: list [A]  ) (b: list [A]  ): list [A] =
    a  match {
      case nil () => b
      case cons (e, s ) => _rec_reverse (s ) (cons (e, b ) )
    }

  def reverse [A] (s: list [A]  ): list [A] =
    _rec_reverse [A] (s ) (nil [A]  ()  )

  def _rec_from_Seq [A] (a: Seq [A]  ) (b: list [A]  ): list [A] =
    a  match {
      case Nil => b
      case (e ):: (s ) => _rec_from_Seq (s ) (cons (e, b )  )
    }

  def from_Seq [A] (a: Seq [A]  ): list [A] =
    reverse (_rec_from_Seq (a ) (nil [A]  () ) )

  def _rec_to_Seq [A] (a: list [A]  ) (b: Seq [A]  ): Seq [A] =
    a  match {
      case nil () => b
      case cons (e, s ) => _rec_to_Seq (s ) (b .+: (e ) )
    }

  def to_Seq [A] (a: list [A]  ): Seq [A] =
    (_rec_to_Seq (a ) (Seq [A]  () )  ) .reverse
}

case class SeqList_ ()  extends SeqList
