package soda.collection

trait MSeq [A]
{

  def   isEmpty : Boolean

  def opt [B] (ifEmpty : B) (ifNonEmpty : NESeq [A] => B) : B =
    this match  {
      case NESeq_ (head, tail) => ifNonEmpty (NESeq_ (head, tail) )
      case x => ifEmpty
    }

}

case class MSeq_ [A] (isEmpty : Boolean) extends MSeq [A]

trait MSeqRec [A]
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold_while [B] (sequence : MSeq [A] ) (current_value : B) (next_value_function : B => A => B) (condition : B => A => Boolean) : B =
    sequence match  {
      case NESeq_ (head, tail) =>
        if ( ! condition (current_value) (head)
        ) current_value
        else _tailrec_fold_while (tail) (next_value_function (current_value) (head) ) (next_value_function) (condition)
      case x => current_value
    }

  def fold_while [B] (sequence : MSeq [A] ) (initial_value : B) (next_value : B => A => B) (condition : B => A => Boolean) : B =
    _tailrec_fold_while (sequence) (initial_value) (next_value) (condition)

}

case class MSeqRec_ [A] () extends MSeqRec [A]

trait ESeq [A]
  extends
    MSeq [A]
{

  lazy val isEmpty = true

}

case class ESeq_ [A] () extends ESeq [A]

trait NEMSeq [A]
  extends
    MSeq [A]
{

  def   head0 : A
  def   tail0 : MSeq [A]

  lazy val isEmpty = false

}

case class NEMSeq_ [A] (head0 : A, tail0 : MSeq [A]) extends NEMSeq [A]

trait NESeq [A]
  extends
    NEMSeq [A]
{

  def   head0 : A
  def   tail0 : MSeq [A]

  lazy val head : A = head0

  lazy val tail : MSeq [A] = tail0

}

case class NESeq_ [A] (head0 : A, tail0 : MSeq [A]) extends NESeq [A]
