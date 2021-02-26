package scopus.collection


trait MSeq[T] {
  val isEmpty: Boolean
  def _head(): Option[T]
  def _tail(): Option[MSeq[T]]

  def head() = _head().get

  def tail() = _tail().get
}


case class ESeq[T] ()
  extends MSeq[T] {

  val isEmpty = true

  def _head() = Option.empty

  def _tail() = Option.empty

}


case class NESeq[T] (head0: T, tail0: MSeq[T])
  extends MSeq[T] {

  val isEmpty = false

  def _head() = Some(head0)

  def _tail() = Some(tail0)

}
