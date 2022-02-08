package soda.collection

trait MSeqTranslator [T]
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_foldLeftSeq [B, C <: B] (sequence: Seq [T], current_value: C, next_value: (B, T ) => C ): C =
    if (sequence.isEmpty
    ) current_value
    else _tailrec_foldLeftSeq (sequence.tail, next_value (current_value, sequence.head ), next_value )

  def foldLeftSeq [B, C <: B] (sequence: Seq [T], initial_value: C, next_value: (B, T ) => C ): C =
    _tailrec_foldLeftSeq (sequence, initial_value, next_value )

  def asMSeq (seq: Seq [T]  ): MSeq [T] =
    {
      lazy val initial_value: MSeq [T] = Min_ () .empty
      def next_value (acc: MSeq [T], elem: T ): MSeq [T] = Min_ () .prepended (acc, elem )
      Min_ () .reverse (foldLeftSeq [MSeq [T], MSeq [T]] (seq, initial_value, next_value ) ) }

  def asSeq (mseq: MSeq [T]  ): Seq [T] =
    {
      lazy val initial_value: Seq [T] = Seq ()
      def next_value (acc: Seq [T], elem: T ): Seq [T] = acc.+: (elem )
      Min_ () .foldLeft (mseq, initial_value, next_value ) .reverse }

}

case class MSeqTranslator_ [T] () extends MSeqTranslator [T]

trait MSeqPair [T]
{

  def   left: MSeq [T]
  def   right: MSeq [T]

}

case class MSeqPair_ [T] (left: MSeq [T], right: MSeq [T]) extends MSeqPair [T]

trait Min [T]
{

  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  lazy val empty: ESeq [T] = ESeq_ ()

  def prepended (s: MSeq [T], e: T ): NESeq [T] =
    NESeq_ (e, s )

  def head (s: NESeq [T]  ): T =
    s.head

  def tail (s: NESeq [T]  ): MSeq [T] =
    s.tail

  def nonEmpty (s: MSeq [T]  ): Boolean =
    ! isEmpty (s )

  def isEmpty (s: MSeq [T]  ): Boolean =
    s.isEmpty

  /* */

  def foldLeftWhile [B, C <: B] (s: MSeq [T], initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
    MSeqRec_ [T] () .fold [B, C] (s, initial_value, next_value, condition )

  def foldLeft [B, C <: B] (s: MSeq [T], initial_value: C, next_value: (B, T ) => C ): C =
    {
      def condition (acc: B, elem: T ): Boolean = true
      foldLeftWhile [B, C] (s, initial_value, next_value, condition ) }

  def reverse (s: MSeq [T]  ): MSeq [T] =
    s.opt (ifEmpty = empty, ifNonEmpty = neseq => reverseNonEmpty (neseq ) )

  def reverseNonEmpty (s: NESeq [T]  ): NESeq [T] =
    {
      lazy val initial_value: NESeq [T] = prepended (empty, s.head )
      def next_value (acc: MSeq [T], elem: T ): NESeq [T] = prepended (acc, elem )
      foldLeft (s.tail, initial_value, next_value ) }

  def length (s: MSeq [T]  ): Int =
    {
      lazy val initial_value: Int = 0
      def next_value (acc: Int, elem: T ): Int = acc + 1
      foldLeft (s, initial_value, next_value ) }

  def indexOf (s: MSeq [T], e: T ): Int =
    {
      lazy val initial_value = IndexFoldTuple_ [T] (0, -1 )
      def next_value (tuple: IndexFoldTuple [T], elem: T ): IndexFoldTuple [T] =
        IndexFoldTuple_ [T] (tuple.index + 1,
          if (elem == e ) tuple.index else tuple.position )
      def condition (tuple: IndexFoldTuple [T], elem: T ): Boolean = tuple.position == -1
      foldLeftWhile (s, initial_value, next_value, condition ) .position }

  def contains (s: MSeq [T], e: T ): Boolean =
    {
      lazy val initial_value: Boolean = false
      def next_value (acc: Boolean, elem: T ): Boolean = elem == e
      def condition (acc: Boolean, elem: T ): Boolean = ! acc
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def at (s: MSeq [T], n: Int ): OptionSD [T] =
    s.opt (
      ifEmpty = NoneSD_ [T] (),
      ifNonEmpty = (neseq =>
        if (n < 0 || n >= length (s )
        ) NoneSD_ [T] ()
        else SomeSD_ [T] (_atNonEmpty (neseq, n )  )
      )
    )

  def _atNonEmpty (xs: NESeq [T], n: Int ): T =
    {
      lazy val initial_value = AtFoldTuple_ [T] (xs.head, -1 )
      def next_value (tuple: AtFoldTuple [T], elem: T ): AtFoldTuple [T] = AtFoldTuple_ [T] (elem, tuple.index + 1 )
      def condition (tuple: AtFoldTuple [T], elem: T ): Boolean = tuple.index < n
      foldLeftWhile (xs, initial_value, next_value, condition ) .elem }

  /* */

  def take (s: MSeq [T], n: Int ): MSeq [T] =
    {
      lazy val initial_value = TakeDropFoldTuple_ [T] (empty, 0 )
      def next_value (tuple: TakeDropFoldTuple [T], elem: T ): TakeDropFoldTuple [T] =
        TakeDropFoldTuple_ [T] (prepended (tuple.seq, elem ), tuple.index + 1 )
      def condition (tuple: TakeDropFoldTuple [T], elem: T ): Boolean = tuple.index < n
      reverse (foldLeftWhile (s, initial_value, next_value, condition ) .seq ) }

  def drop (s: MSeq [T], n: Int ): MSeq [T] =
    {
      lazy val initial_value = TakeDropFoldTuple_ [T] (s, 0 )
      def next_value (tuple: TakeDropFoldTuple [T], elem: T ): TakeDropFoldTuple [T] =
        tuple.seq.opt (
          ifEmpty = TakeDropFoldTuple_ [T] (tuple.seq, tuple.index + 1 ),
          ifNonEmpty = neseq => TakeDropFoldTuple_ [T] (neseq.tail, tuple.index + 1 )
        )
      def condition (tuple: TakeDropFoldTuple [T], elem: T ): Boolean =
        tuple.index < n
      foldLeftWhile (s, initial_value, next_value, condition ) .seq }

  def takeWhile (s: MSeq [T], p: (T => Boolean )  ): MSeq [T] = reverse (spanRevRec (s, p ) .left )

  def dropWhile (s: MSeq [T], p: (T => Boolean )  ): MSeq [T] = spanRevRec (s, p ) .right

  def splitAt (s: MSeq [T], n: Int ): MSeqPair [T] = MSeqPair_ (take (s, n ), drop (s, n )  )

  def span (s: MSeq [T], p: (T => Boolean ) ): MSeqPair [T] =
    {
      lazy val pair = spanRevRec (s, p )
      MSeqPair_ (reverse (pair.left ), pair.right ) }

  /* */

  def appended (s: MSeq [T], e: T ): MSeq [T] = reverse (prepended (reverse (s ), e )  )

  def last (s: NESeq [T]  ): T = reverseNonEmpty (s ) .head

  def concat (s0: MSeq [T], s1: MSeq [T]  ): MSeq [T] =
    {
      lazy val initial_value: MSeq [T] = s1
      def next_value (acc: MSeq [T], elem: T ): MSeq [T] = prepended (acc, elem )
      lazy val s0rev = reverse (s0 )
      foldLeft (s0rev, initial_value, next_value ) }

  def slice (s: MSeq [T], from: Int, until: Int ): MSeq [T] = take (drop (s, from ), until - from )

  /* */

  def forall (s: MSeq [T], p: (T => Boolean )  ): Boolean =
    {
      lazy val initial_value = true
      def next_value (acc: Boolean, elem: T ): Boolean = acc && p (elem )
      def condition (acc: Boolean, elem: T ): Boolean = acc
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def exists (s: MSeq [T], p: (T => Boolean ) ): Boolean =
    {
      lazy val initial_value = false
      def next_value (acc: Boolean, elem: T ): Boolean = acc || p (elem )
      def condition (acc: Boolean, elem: T ): Boolean = ! acc
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def find (s: MSeq [T], p: (T => Boolean )  ): OptionSD [T] =
    {
      lazy val initial_value = NoneSD_ [T] ()
      def next_value (acc: OptionSD [T], elem: T ): OptionSD [T] =
        if (p (elem ) ) SomeSD_ [T] (elem ) else NoneSD_ [T] ()
      def condition (acc: OptionSD [T], elem: T ): Boolean = acc.isEmpty
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def filter (s: MSeq [T], p: (T => Boolean )  ): MSeq [T] =
    {
      lazy val initial_value = empty
      def next_value (acc: MSeq [T], elem: T ): MSeq [T] =
        if (p (elem )
        ) prepended (acc, elem )
        else acc
      reverse (foldLeft (s, initial_value, next_value ) ) }

  def map0 (s: MSeq [T], f: (T => T )  ): MSeq [T] =
    {
      lazy val initial_value = empty
      def next_value (acc: MSeq [T], elem: T ): MSeq [T] =
        prepended (acc, f (elem )  )
      reverse (foldLeft (s, initial_value, next_value ) ) }

  /**
   * <pre>
   * def foldLeft [B] (z: B) (op: (B, A) -> B): B =
   * . var result = z
   * . it = iterator
   * . while (it.hasNext) {
   * . . result = op (result, it.next ())
   * . end
   * . result
   * end
   * </pre>
   */
  def foldLeft0 (mseq: MSeq [T]  ): (MSeq [T], ((MSeq [T], T ) => MSeq [T]  )  ) => MSeq [T] =
    (initial_value: MSeq [T], next_value: ((MSeq [T], T ) => MSeq [T]  )  ) => foldLeft (mseq, initial_value, next_value )

  /* */

  def spanRevRec (s0: MSeq [T], p: T => Boolean ): MSeqPair [T] =
    {
      lazy val result = MSeqPair_ (pair.right, pair.left )
      lazy val pair = foldLeftWhile (s0, initial_value, next_value, condition )
      lazy val initial_value = SpanRevFoldTuple_ [T] (s0, empty, true )
      def next_value (tuple: SpanRevFoldTuple [T], elem: T ): SpanRevFoldTuple [T] =
        {
          lazy val left = tuple.left
          lazy val right = tuple.right
          lazy val result = left.opt (
            ifEmpty = SpanRevFoldTuple_ [T] (left, right, false ),
            ifNonEmpty = neleft => _aux_next_value (tuple, p, neleft )
          )
          result }
      def condition (tuple: SpanRevFoldTuple [T], elem: T ): Boolean = tuple.taking
      result }

  def _aux_next_value (tuple: SpanRevFoldTuple [T], p: T => Boolean, neleft: NESeq [T] ): SpanRevFoldTuple [T] =
    {
      lazy val left = tuple.left
      lazy val right = tuple.right
      lazy val e = neleft.head
      lazy val new_taking = p (e )
      lazy val new_tuple =
        if (new_taking
        ) SpanRevFoldTuple_ [T] (neleft.tail, prepended (right, e ), new_taking )
        else SpanRevFoldTuple_ [T] (neleft, right, new_taking )
      new_tuple }

}

case class Min_ [T] () extends Min [T]

trait IndexFoldTuple [T]
{

  def   index: Int
  def   position: Int

}

case class IndexFoldTuple_ [T] (index: Int, position: Int) extends IndexFoldTuple [T]

trait AtFoldTuple [T]
{

  def   elem: T
  def   index: Int

}

case class AtFoldTuple_ [T] (elem: T, index: Int) extends AtFoldTuple [T]

trait TakeDropFoldTuple [T]
{

  def   seq: MSeq [T]
  def   index: Int

}

case class TakeDropFoldTuple_ [T] (seq: MSeq [T], index: Int) extends TakeDropFoldTuple [T]

trait SpanRevFoldTuple [T]
{

  def   left: MSeq [T]
  def   right: MSeq [T]
  def   taking: Boolean

}

case class SpanRevFoldTuple_ [T] (left: MSeq [T], right: MSeq [T], taking: Boolean) extends SpanRevFoldTuple [T]
