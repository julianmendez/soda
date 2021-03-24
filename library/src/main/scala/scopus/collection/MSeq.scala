package scopus.collection


trait MSeq [ T ] {
  def isEmpty: Boolean
  def _head (  ) : Option [ T ]
  def _tail (  ) : Option [ MSeq [ T ]  ]

  def head (  ) = _head (  ) .get

  def tail (  ) = _tail (  ) .get

  def foldLeftWhile [ B ]  ( initval: B , op: ( B , T ) => B , cond: ( B , T ) => Boolean ) : B = {
    lazy val result = rec ( this , initval , op , cond )

    import scala.annotation.tailrec
        @tailrec
    def rec [ B ]  ( seq: MSeq [ T ]  , acc: B , op: ( B , T ) => B , cond: ( B , T ) => Boolean ) : B =
      if ( seq.isEmpty
      ) acc
      else {
        lazy val ( elem , rest ) = ( seq.head (  )  , seq.tail (  )  )
        if ( ! cond ( acc , elem )
        ) acc
        else rec ( rest , op ( acc , elem )  , op , cond )
      }

    result
  }

}


case class ESeq [ T ] (  )
  extends MSeq [ T ] {

  lazy val isEmpty = true

  def _head (  ) = Option.empty

  def _tail (  ) = Option.empty

}


case class NESeq [ T ] ( head0: T , tail0: MSeq [ T ]  )
  extends MSeq [ T ] {

  lazy val isEmpty = false

  def _head (  ) = Some ( head0 )

  def _tail (  ) = Some ( tail0 )

}
