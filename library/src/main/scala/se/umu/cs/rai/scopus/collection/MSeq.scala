package se.umu.cs.rai.scopus.collection


trait MSeq[T] {
  val isEmpty: Boolean

  def head(): T

  def tail(): MSeq[T]
}


case class ESeq[T] ()
  extends MSeq[T] {

  val isEmpty = true

  def head(): T = throw new UnsupportedOperationException

  def tail(): MSeq[T] = throw new UnsupportedOperationException

}


case class NESeq[T] (head0: T, tail0: MSeq[T])
  extends MSeq[T] {

  val isEmpty = false

  def head() = head0

  def tail() = tail0

}