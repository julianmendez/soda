/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This is an Option implemented without exceptions.
 */
trait OptionSD [T] {

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B

  def toOption: Option [T]

  lazy val isEmpty: Boolean =
    open (
      ifEmpty = true, ifNonEmpty = element => false
    )

  def map [B]  (mapping: T => B ): OptionSD [B] =
    open (
      ifEmpty = NoneSD [B]  (), ifNonEmpty = element => SomeSD [B]  (mapping (element )  )
    )
}

case class NoneSD [T] () extends OptionSD [T] {

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B = ifEmpty

  lazy val toOption: None.type = None
}

case class SomeSD [T] (element: T ) extends OptionSD [T] {

  lazy val get: T = element

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B = ifNonEmpty (element )

  lazy val toOption: Some [T] = Some [T]  (element )
}

case class OptionSDBuilder [T]  () {

  def build (opt: Option [T]  ): OptionSD [T] =
    if (opt.isEmpty
    ) NoneSD [T]  ()
    else SomeSD [T]  (opt.get )

}