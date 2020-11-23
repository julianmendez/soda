package se.umu.cs.rai.scopus.collection


trait MSeq[T] {
  val isEmpty: Boolean

  val head: T

  val tail: MSeq[T]
}


case class ESeq[T] ()
  extends MSeq[T] {

  val isEmpty = true

  val head: T = throw new UnsupportedOperationException

  val tail: MSeq[T] = throw new UnsupportedOperationException

}


case class NESeq[T] (head: T, tail: MSeq[T])
  extends MSeq[T] {

  val isEmpty = false

}
