package soda.collection


trait MSeq [T] {

  def isEmpty: Boolean

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B
}

trait MSeqRec [T] {

  def fold [B, C <: B]  (mseq: MSeq [T], initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
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
      rec (mseq, initial_value, next_value, condition ) }
}

case class MSeqRec_ [T]  () extends MSeqRec [T]

trait ESeq [T] extends MSeq [T] {

  lazy val isEmpty = true

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifEmpty
}

case class ESeq_ [T]  () extends ESeq [T]

trait NESeq [T] extends MSeq [T] {

  def head0: T

  def tail0: MSeq [T]

  lazy val isEmpty = false

  def opt [B]  (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifNonEmpty (this )

  def head (): T = head0

  def tail (): MSeq [T] = tail0
}

case class NESeq_ [T]  (head0: T, tail0: MSeq [T]  ) extends NESeq [T]
