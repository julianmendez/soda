/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This is an Option implemented without exceptions.
 */
trait OptionSD [A] {

  def opt [B]  (ifEmpty: B, ifNonEmpty: A => B ): B

  lazy val isEmpty: Boolean =
    opt (ifEmpty = true, ifNonEmpty = element => false )

  lazy val isDefined: Boolean = ! isEmpty

  lazy val nonEmpty: Boolean = ! isEmpty

  lazy val toOption: Option [A] =
    opt (ifEmpty = None, ifNonEmpty = element => Some [A]  (element )  )

  lazy val toSeq: Seq [A] =
    opt (ifEmpty = Seq (), ifNonEmpty = element => Seq (element )  )

  def getOrElse (default: A ): A =
    opt (ifEmpty = default, ifNonEmpty = element => element )

  def fold [B]  (ifEmpty: B, f: A => B ): B =
    opt (ifEmpty, f )

  def map [B]  (mapping: A => B ): OptionSD [B] =
    opt (ifEmpty = NoneSD [B]  (), ifNonEmpty = element => SomeSD [B]  (mapping (element )  )
    )

  def flatMap [B]  (mapping: A => OptionSD [B]  ): OptionSD [B] =
    opt (ifEmpty = NoneSD [B]  (), ifNonEmpty = element => mapping (element )
    )

  def filter (predicate: A => Boolean ): OptionSD [A] =
    opt (ifEmpty = this, ifNonEmpty = element => if (predicate (element ) ) this else NoneSD [A]  ()
    )
}

case class NoneSD [A] () extends OptionSD [A] {

  def opt [B]  (ifEmpty: B, ifNonEmpty: A => B ): B = ifEmpty
}

case class SomeSD [A] (element: A ) extends OptionSD [A] {

  lazy val get: A = element

  def opt [B]  (ifEmpty: B, ifNonEmpty: A => B ): B = ifNonEmpty (element )
}

case class OptionSDBuilder [A]  () {

  def build (option: Option [A]  ): OptionSD [A] =
    if (option.isEmpty
    ) NoneSD [A]  ()
    else SomeSD [A]  (option.get )
}
