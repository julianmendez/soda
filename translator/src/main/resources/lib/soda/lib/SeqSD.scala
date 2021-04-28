/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This is a Seq implemented without exceptions.
 */
trait SeqSD [T] {

  def open [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [T] => B ): B

  def toSeq: Seq [T]

  def reverse: SeqSD [T]

  override
  lazy val toString: String = toSeq.toString

}

case class EmptySeqSD [T]  () extends SeqSD [T] {

  def open [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [T] => B ): B = ifEmpty

  lazy val toSeq: Seq [T] = Seq [T]  ()

  lazy val reverse: EmptySeqSD [T] = this

}

trait NonEmptySeqSD [T] extends SeqSD [T] {

  def open [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [T] => B ): B = ifNonEmpty (this )

  lazy val head: T = toSeq.head

  lazy val tail: SeqSD [T] = SeqSDBuilder [T]  () .build (toSeq.tail )

  lazy val reverse: NonEmptySeqSD [T] = _NonEmptySeqSD (toSeq.reverse )

}

case class _NonEmptySeqSD [T]  (toSeq: Seq [T]  ) extends NonEmptySeqSD [T]

case class SeqSDBuilder [T]  () {

  def build (seq: Seq [T]  ): SeqSD [T] =
    if (seq.isEmpty
    ) EmptySeqSD [T]  ()
    else _NonEmptySeqSD [T]  (seq )

}
