package soda.collection

trait MSeq [T]
{

  def   isEmpty: Boolean

  def opt [B] (ifEmpty: B ) (ifNonEmpty: NESeq [T] => B ): B =
    this match  {
      case ESeq_ () => ifEmpty
      case NESeq_ (head, tail ) => ifNonEmpty (NESeq_ (head, tail ) )
    }

}

case class MSeq_ [T] (isEmpty: Boolean) extends MSeq [T]

trait MSeqRec [T]
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_fold_while [B] (sequence: MSeq [T] ) (current_value: B ) (next_value_function: B => T => B ) (condition: B => T => Boolean ): B =
    sequence match  {
      case ESeq_ () => current_value
      case NESeq_ (head, tail ) =>
        if (! condition (current_value ) (head )
        ) current_value
        else _tailrec_fold_while (tail ) (next_value_function (current_value ) (head ) ) (next_value_function ) (condition )
      }

  def fold_while [B] (sequence: MSeq [T] ) (initial_value: B ) (next_value: B => T => B ) (condition: B => T => Boolean ): B =
    _tailrec_fold_while (sequence ) (initial_value ) (next_value ) (condition )

}

case class MSeqRec_ [T] () extends MSeqRec [T]

trait ESeq [T]
  extends
    MSeq [T]
{

  lazy val isEmpty = true

}

case class ESeq_ [T] () extends ESeq [T]

trait NEMSeq [T]
  extends
    MSeq [T]
{

  def   head0: T
  def   tail0: MSeq [T]

  lazy val isEmpty = false

}

case class NEMSeq_ [T] (head0: T, tail0: MSeq [T]) extends NEMSeq [T]

trait NESeq [T]
  extends
    NEMSeq [T]
{

  def   head0: T
  def   tail0: MSeq [T]

  lazy val head: T = head0

  lazy val tail: MSeq [T] = tail0

}

case class NESeq_ [T] (head0: T, tail0: MSeq [T]) extends NESeq [T]
