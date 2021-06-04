package soda.collection


trait MSeq [T] {
  def isEmpty: Boolean

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B

  def foldLeftWhile [B, C <: B]  (initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (seq: MSeq [T], acc: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
        if (seq.isEmpty
        ) acc
        else
          {
            lazy val neseq = seq.opt (ifEmpty = None, ifNonEmpty = (x => Some (x )  )  ) .get
            if (! condition (acc, neseq.head ()  ) ) acc else rec (neseq.tail (), next_value (acc, neseq.head ()  ), next_value, condition ) }
      rec (this, initial_value, next_value, condition ) }
}


case class ESeq [T] () extends MSeq [T] {

  lazy val isEmpty = true

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifEmpty
}


case class NESeq [T] (head0: T, tail0: MSeq [T]  ) extends MSeq [T] {

  lazy val isEmpty = false

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifNonEmpty (this )

  def head (): T = head0

  def tail (): MSeq [T] = tail0
}
