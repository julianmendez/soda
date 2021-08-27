/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This is a Seq implemented without exceptions.
 */
trait SeqSD [A] {

  def opt [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [A] => B ): B

  def toSeq: Seq [A]

  def reverse: SeqSD [A]
}

trait EmptySeqSD [A] extends SeqSD [A] {

  def opt [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [A] => B ): B = ifEmpty

  lazy val toSeq: Seq [A] = Seq [A]  ()

  lazy val reverse: EmptySeqSD [A] = this
}

case class EmptySeqSD_ [A]  () extends EmptySeqSD [A]

trait NonEmptySeqSD [A] extends SeqSD [A] {

  def opt [B]  (ifEmpty: B, ifNonEmpty: NonEmptySeqSD [A] => B ): B = ifNonEmpty (this )

  lazy val head: A = toSeq.head

  lazy val tail: SeqSD [A] = SeqSDBuilder_ [A]  () .build (toSeq.tail )

  lazy val reverse: NonEmptySeqSD [A] = NonEmptySeqSD_ (toSeq.reverse )
}

case class NonEmptySeqSD_ [A]  (toSeq: Seq [A]  ) extends NonEmptySeqSD [A]

trait SeqSDBuilder [A] {

  def build (seq: Seq [A]  ): SeqSD [A] =
    if (seq.isEmpty
    ) EmptySeqSD_ [A]  ()
    else NonEmptySeqSD_ [A]  (seq )
}

case class SeqSDBuilder_ [A]  () extends SeqSDBuilder [A]
