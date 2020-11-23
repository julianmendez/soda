package se.umu.cs.rai.scopus.collection


case class Elem(e: Long)

object Elem {}


trait MSeq {

  def isEmpty: Boolean

  def head: Elem

  def tail: MSeq

}


case class ESeq() extends MSeq {

  override def isEmpty: Boolean = true

  override def head: Elem = throw new UnsupportedOperationException

  override def tail: MSeq = throw new UnsupportedOperationException

}

object ESeq {}


case class NESeq(head: Elem, tail: MSeq) extends MSeq {

  override def isEmpty: Boolean = false

}

object NESeq {}

