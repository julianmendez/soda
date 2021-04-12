package soda.collection


trait MSeq [T] {
  def isEmpty: Boolean

  def asNonEmpty: Option [NESeq [T]]

  def foldLeftWhile [B, C <: B]  (initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C = {
    lazy val result = rec (this, initial_value, next_value, condition )

    import scala.annotation.tailrec
        @tailrec
    def rec (seq: MSeq [T], acc: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C = {
      lazy val maybe_neseq = seq.asNonEmpty

      if (maybe_neseq.isEmpty
      ) acc
      else {
        lazy val neseq = maybe_neseq.get
        lazy val (elem, rest ) = (neseq.head (), neseq.tail ()  )
        if (! condition (acc, elem )
        ) acc
        else rec (rest, next_value (acc, elem ), next_value, condition )
      }
    }

    result
  }

}


case class ESeq [T] () extends MSeq [T] {

  lazy val isEmpty = true

  lazy val asNonEmpty: Option [NESeq [T]] = None

}


case class NESeq [T] (head0: T, tail0: MSeq [T]  ) extends MSeq [T] {

  lazy val isEmpty = false

  lazy val asNonEmpty: Option [NESeq [T]] = Some (this )

  def head (): T = head0

  def tail (): MSeq [T] = tail0

}
