package se.umu.cs.rai.scopus.collection


case class Elem (e: Long)

trait MSeq {
  val isEmpty: Boolean

  val head: Elem

  val tail: MSeq
}


case class ESeq ()
  extends MSeq {

  val isEmpty = true

  val head: Elem = throw new UnsupportedOperationException

  val tail: MSeq = throw new UnsupportedOperationException

}


case class NESeq (head: Elem, tail: MSeq)
  extends MSeq {

  val isEmpty = false

}
