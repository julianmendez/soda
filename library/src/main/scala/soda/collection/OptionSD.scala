package soda.collection


trait OptionSD [T] {
  def isEmpty: Boolean

  def open [B]  (ifEmpty: B, ifNonEmpty: SomeSD [T] => B ): B

}


case class NoneSD [T] () extends OptionSD [T] {

  lazy val isEmpty = true

  def open [B]  (ifEmpty: B, ifNonEmpty: SomeSD [T] => B ): B = ifEmpty

}


case class SomeSD [T] (element: T ) extends OptionSD [T] {

  lazy val isEmpty = false

  def open [B]  (ifEmpty: B, ifNonEmpty: SomeSD [T] => B ): B = ifNonEmpty (this )

  lazy val get: T = element

}
