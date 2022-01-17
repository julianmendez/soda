package soda.collection

trait MSeq [T] {

  def isEmpty: Boolean

  def opt [B] (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B

}

trait MSeqRec [T] {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_fold [B, C <: B] (sequence: MSeq [T], current_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
    if (sequence.isEmpty
    ) current_value
    else
      {
        lazy val neseq = sequence.opt (ifEmpty = None, ifNonEmpty = (x => Some (x )  )  ) .get
        if (! condition (current_value, neseq.head ()  ) ) current_value else _tailrec_fold (neseq.tail (), next_value (current_value, neseq.head ()  ), next_value, condition ) }

  def fold [B, C <: B] (sequence: MSeq [T], initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
    _tailrec_fold (sequence, initial_value, next_value, condition )

}

case class MSeqRec_ [T] ()
  extends MSeqRec [T]

trait ESeq [T]
  extends MSeq [T] {

  lazy val isEmpty = true

  def opt [B] (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifEmpty

}

case class ESeq_ [T] ()
  extends ESeq [T]

trait NEMSeq [T]
  extends MSeq [T] {

  def head0: T

  def tail0: MSeq [T]

}

trait NESeq [T]
  extends NEMSeq [T] {

  lazy val isEmpty = false

  def opt [B] (ifEmpty: B, ifNonEmpty: NESeq [T] => B ): B = ifNonEmpty (this )

  def head (): T = head0

  def tail (): MSeq [T] = tail0

}

case class NESeq_ [T] (head0: T, tail0: MSeq [T]  )
  extends NESeq [T]
