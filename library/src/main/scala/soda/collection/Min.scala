package scopus.collection


case class MSeqTranslator [ T ]  (  ) {

  def foldLeftSeq [ B ]  ( seq: Seq [ T ]  , initial_value: B , next_value: ( B , T ) => B ) : B = {
    lazy val result = rec ( seq , initial_value , next_value )

    import scala.annotation.tailrec
        @tailrec
    def rec [ B ]  ( seq: Seq [ T ]  , acc: B , next_value: ( B , T ) => B ) : B =
      if ( seq.isEmpty
      ) acc
      else rec ( seq.tail , next_value ( acc , seq.head )  , next_value )

    result
  }

  def asMSeq ( seq: Seq [ T ]  ) : MSeq [ T ] = {
    lazy val result = Min (  ) .reverse (
      foldLeftSeq [ MSeq [ T ]  ]  ( seq , initial_value , next_value ) )

    lazy val initial_value: MSeq [ T ] = Min (  ) .empty

    def next_value ( acc: MSeq [ T ]  , elem: T ) : MSeq [ T ] = Min (  ) .prepended ( acc , elem )

    result
  }

  def asSeq ( mseq: MSeq [ T ]  ) : Seq [ T ] = {
    lazy val result = Min (  ) .foldLeft ( mseq , initial_value , next_value ) .reverse

    lazy val initial_value: Seq [ T ] = Seq (  )

    def next_value ( acc: Seq [ T ]  , elem: T ) : Seq [ T ] = acc.+: ( elem )

    result
  }

}


case class Min [ T ]  (  ) {

  lazy val empty: MSeq [ T ] = ESeq (  )

  def prepended ( s: MSeq [ T ]  , e: T ) : MSeq [ T ] = NESeq ( e , s )

  def head ( s: MSeq [ T ]  ) : T = s.head (  )

  def tail ( s: MSeq [ T ]  ) : MSeq [ T ] = s.tail (  )

  def nonEmpty ( s: MSeq [ T ]  ) : Boolean = ! isEmpty ( s )

  def isEmpty ( s: MSeq [ T ]  ) : Boolean = s.isEmpty

  /* */

  def foldLeftWhile [ B ]  ( s: MSeq [ T ]  , initial_value: B , next_value: ( B , T ) => B , condition: ( B , T ) => Boolean ) : B =
    s.foldLeftWhile [ B ]  ( initial_value , next_value , condition )

  def foldLeft [ B ]  ( s: MSeq [ T ]  , initial_value: B , next_value: ( B , T ) => B ) : B = {
    def condition ( acc: B , elem: T ) : Boolean = true

    foldLeftWhile [ B ]  ( s , initial_value , next_value , condition )
  }

  def reverse ( s: MSeq [ T ]  ) : MSeq [ T ] = {
    lazy val initial_value: MSeq [ T ] = empty

    def next_value ( acc: MSeq [ T ]  , elem: T ) : MSeq [ T ] = prepended ( acc , elem )

    foldLeft ( s , initial_value , next_value )
  }

  def length ( s: MSeq [ T ]  ) : Int = {
    lazy val initial_value: Int = 0

    def next_value ( acc: Int , elem: T ) : Int = acc + 1

    foldLeft ( s , initial_value , next_value )
  }

  def indexOf ( s: MSeq [ T ]  , e: T ) : Int = {
    lazy val initial_value = FoldTuple ( 0 , -1 )

    def next_value ( tuple: FoldTuple , elem: T ) : FoldTuple =
      FoldTuple ( tuple.index + 1 ,        if ( elem == e ) tuple.index else tuple.position )

    def condition ( tuple: FoldTuple , elem: T ) : Boolean = tuple.position == -1

    case class FoldTuple ( index: Int , position: Int )

    foldLeftWhile ( s , initial_value , next_value , condition ) .position
  }

  def contains ( s: MSeq [ T ]  , e: T ) : Boolean = {
    lazy val initial_value: Boolean = false

    def next_value ( acc: Boolean , elem: T ) : Boolean = elem == e

    def condition ( acc: Boolean , elem: T ) : Boolean = ! acc

    foldLeftWhile ( s , initial_value , next_value , condition )
  }

  def at ( s: MSeq [ T ]  , n: Int ) : T = {
    lazy val result = if ( isEmpty ( s ) || n < 0 || n >= length ( s ) ) None.get else atNonEmpty ( s , n )

    def atNonEmpty ( xs: MSeq [ T ]  , n: Int ) : T = {
      lazy val initial_value = FoldTuple ( head ( xs )  , -1 )

      def next_value ( tuple: FoldTuple , elem: T ) : FoldTuple = FoldTuple ( elem , tuple.index + 1 )

      def condition ( tuple: FoldTuple , elem: T ) : Boolean = tuple.index < n

      case class FoldTuple ( elem: T , index: Int )

      foldLeftWhile ( xs , initial_value , next_value , condition ) .elem
    }

    result
  }

  /* */

  def take ( s: MSeq [ T ]  , n: Int ) : MSeq [ T ] = {
    lazy val result = reverse (
        foldLeftWhile ( s , initial_value , next_value , condition ) .seq )

    lazy val initial_value = FoldTuple ( empty , 0 )

    def next_value ( tuple: FoldTuple , elem: T ) : FoldTuple =
      FoldTuple ( prepended ( tuple.seq , elem )  , tuple.index + 1 )

    def condition ( tuple: FoldTuple , elem: T ) : Boolean =
      tuple.index < n

    case class FoldTuple ( seq: MSeq [ T ]  , index: Int )

    result
  }

  def drop ( s: MSeq [ T ]  , n: Int ) : MSeq [ T ] = {
    lazy val result = foldLeftWhile ( s , initial_value , next_value , condition ) .seq

    lazy val initial_value = FoldTuple ( s , 0 )

    def next_value ( tuple: FoldTuple , elem: T ) : FoldTuple =
      FoldTuple ( tail ( tuple.seq )  , tuple.index + 1 )

    def condition ( tuple: FoldTuple , elem: T ) : Boolean =
      tuple.index < n

    case class FoldTuple ( seq: MSeq [ T ]  , index: Int )

    result
  }

  def takeWhile ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : MSeq [ T ] = reverse ( spanRevRec ( s , p ) ._1 )

  def dropWhile ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : MSeq [ T ] = spanRevRec ( s , p ) ._2

  def splitAt ( s: MSeq [ T ]  , n: Int ) : ( MSeq [ T ]  , MSeq [ T ]  ) = ( take ( s , n )  , drop ( s , n )  )

  def span ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : ( MSeq [ T ]  , MSeq [ T ]  ) = {
    lazy val pair = spanRevRec ( s , p )
    ( reverse ( pair._1 )  , pair._2 )
  }

  /* */

  def appended ( s: MSeq [ T ]  , e: T ) : MSeq [ T ] = reverse ( prepended ( reverse ( s )  , e )  )

  def last ( s: MSeq [ T ]  ) : T = head ( reverse ( s )  )

  def concat ( s0: MSeq [ T ]  , s1: MSeq [ T ]  ) : MSeq [ T ] = {
    lazy val initial_value: MSeq [ T ] = s1

    def next_value ( acc: MSeq [ T ]  , elem: T ) : MSeq [ T ] = prepended ( acc , elem )

    lazy val s0rev = reverse ( s0 )

    foldLeft ( s0rev , initial_value , next_value )
  }

  def slice ( s: MSeq [ T ]  , from: Int , until: Int ) : MSeq [ T ] = take ( drop ( s , from )  , until - from )

  /* */

  def forall ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : Boolean = {
    lazy val result = foldLeftWhile ( s , initial_value , next_value , condition )

    lazy val initial_value = true

    def next_value ( acc: Boolean , elem: T ) : Boolean = acc && p ( elem )

    def condition ( acc: Boolean , elem: T ) : Boolean = acc

    result
  }

  def exists ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : Boolean = {
    lazy val result = foldLeftWhile ( s , initial_value , next_value , condition )

    lazy val initial_value = false

    def next_value ( acc: Boolean , elem: T ) : Boolean = acc || p ( elem )

    def condition ( acc: Boolean , elem: T ) : Boolean = ! acc

    result
  }

  def find ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : Option [ T ] = {
      lazy val result = foldLeftWhile ( s , initial_value , next_value , condition )

      lazy val initial_value = None

      def next_value ( acc: Option [ T ]  , elem: T ) : Option [ T ] =
        if ( p ( elem ) ) Some ( elem ) else None

      def condition ( acc: Option [ T ]  , elem: T ) : Boolean = acc.isEmpty

      result
    }

  def filter ( s: MSeq [ T ]  , p: ( T => Boolean )  ) : MSeq [ T ] = {
      lazy val result = reverse ( foldLeft ( s , initial_value , next_value ) )

      lazy val initial_value = empty

      def next_value ( acc: MSeq [ T ]  , elem: T ) : MSeq [ T ] =
        if ( p ( elem )
        ) prepended ( acc , elem )
        else acc

      result
    }

  def map0 ( s: MSeq [ T ]  , f: ( T => T )  ) : MSeq [ T ] = {
    lazy val result = reverse ( foldLeft ( s , initial_value , next_value ) )

    lazy val initial_value = empty

    def next_value ( acc: MSeq [ T ]  , elem: T ) : MSeq [ T ] =
      prepended ( acc , f ( elem )  )

    result
  }

  /**
   * <pre>
   * def foldLeft[B](z: B)(op: (B, A) -> B): B = {
   * . var result = z
   * . it = iterator
   * . while (it.hasNext) {
   * . . result = op(result, it.next())
   * . }
   * . result
   * }
   * </pre>
   */
  def foldLeft0 ( mseq: MSeq [ T ]  ) : ( MSeq [ T ]  , (  ( MSeq [ T ]  , T ) => MSeq [ T ]  )  ) => MSeq [ T ] =
    ( initial_value: MSeq [ T ]  , next_value: (  ( MSeq [ T ]  , T ) => MSeq [ T ]  )  ) => foldLeft ( mseq , initial_value , next_value )

  /* */

  def spanRevRec ( s0: MSeq [ T ]  , p: T => Boolean ) : ( MSeq [ T ]  , MSeq [ T ]  ) = {
    lazy val result = ( pair.right , pair.left )
    lazy val pair = foldLeftWhile ( s0 , initial_value , next_value , condition )

    lazy val initial_value = FoldTuple ( s0 , empty , true )

    def next_value ( tuple: FoldTuple , elem: T ) : FoldTuple = {
      lazy val left = tuple.left
      lazy val right = tuple.right
      if ( isEmpty ( left )
      ) FoldTuple ( left , right , false )
      else {
        lazy val e = head ( left )
        lazy val new_taking = p ( e )

        if ( new_taking
        ) FoldTuple ( tail ( left )  , prepended ( right , e )  , new_taking )
        else FoldTuple ( left , right , new_taking )
      }
    }

    def condition ( tuple: FoldTuple , elem: T ) : Boolean = tuple.taking

    case class FoldTuple ( left: MSeq [ T ]  , right: MSeq [ T ]  , taking: Boolean )

    result
  }

}