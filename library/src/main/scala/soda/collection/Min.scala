package soda.collection


trait MSeqTranslator [T] {

  def foldLeftSeq [B, C <: B]  (seq: Seq [T], initial_value: C, next_value: (B, T ) => C ): C =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (seq: Seq [T], acc: C, next_value: (B, T ) => C ): C =
        if (seq.isEmpty
        ) acc
        else rec (seq.tail, next_value (acc, seq.head ), next_value )

      rec (seq, initial_value, next_value ) }

  def asMSeq (seq: Seq [T]  ): MSeq [T] =
    {
      lazy val initial_value: MSeq [T] = MinImpl () .empty
      def next_value (acc: MSeq [T], elem: T ): MSeq [T] = MinImpl () .prepended (acc, elem )
      MinImpl () .reverse (foldLeftSeq [MSeq [T], MSeq [T]]  (seq, initial_value, next_value ) ) }

  def asSeq (mseq: MSeq [T]  ): Seq [T] =
    {
      lazy val initial_value: Seq [T] = Seq ()
      def next_value (acc: Seq [T], elem: T ): Seq [T] = acc.+: (elem )
      MinImpl () .foldLeft (mseq, initial_value, next_value ) .reverse }
}

case class MSeqTranslatorImpl [T]  () extends MSeqTranslator [T]

case class MSeqPair [T]  (left: MSeq [T], right: MSeq [T]  )

trait Min [T] {
  import soda.lib.OptionSD
  import soda.lib.SomeElem
  import soda.lib.NoElem

  lazy val empty: ESeq [T] = ESeqImpl ()

  def prepended (s: MSeq [T], e: T ): NESeq [T] = NESeqImpl (e, s )

  def head (s: NESeq [T]  ): T = s.head ()

  def tail (s: NESeq [T]  ): MSeq [T] = s.tail ()

  def nonEmpty (s: MSeq [T]  ): Boolean = ! isEmpty (s )

  def isEmpty (s: MSeq [T]  ): Boolean = s.isEmpty

  /* */

  def foldLeftWhile [B, C <: B]  (s: MSeq [T], initial_value: C, next_value: (B, T ) => C, condition: (B, T ) => Boolean ): C =
    s.foldLeftWhile [B, C]  (initial_value, next_value, condition )

  def foldLeft [B, C <: B]  (s: MSeq [T], initial_value: C, next_value: (B, T ) => C ): C =
    {
      def condition (acc: B, elem: T ): Boolean = true
      foldLeftWhile [B, C]  (s, initial_value, next_value, condition ) }

  def reverse (s: MSeq [T]  ): MSeq [T] =
    s.opt (ifEmpty = empty, ifNonEmpty = (neseq => reverseNonEmpty (neseq )  )  )

  def reverseNonEmpty (s: NESeq [T]  ): NESeq [T] =
    {
      lazy val initial_value: NESeq [T] = prepended (empty, s.head ()  )
      def next_value (acc: MSeq [T], elem: T ): NESeq [T] = prepended (acc, elem )
      foldLeft (s.tail (), initial_value, next_value ) }

  def length (s: MSeq [T]  ): Int =
    {
      lazy val initial_value: Int = 0
      def next_value (acc: Int, elem: T ): Int = acc + 1
      foldLeft (s, initial_value, next_value ) }

  def indexOf (s: MSeq [T], e: T ): Int =
    {
      lazy val initial_value = IndexFoldTuple [T]  (0, -1 )
      def next_value (tuple: IndexFoldTuple [T], elem: T ): IndexFoldTuple [T] =
        IndexFoldTuple [T]  (tuple.index + 1, if (elem == e ) tuple.index else tuple.position )
      def condition (tuple: IndexFoldTuple [T], elem: T ): Boolean = tuple.position == -1
      foldLeftWhile (s, initial_value, next_value, condition ) .position }

  def contains (s: MSeq [T], e: T ): Boolean =
    {
      lazy val initial_value: Boolean = false
      def next_value (acc: Boolean, elem: T ): Boolean = elem == e
      def condition (acc: Boolean, elem: T ): Boolean = ! acc
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def at (s: MSeq [T], n: Int ): OptionSD [T] =
    s.opt (ifEmpty = NoElem [T]  (), ifNonEmpty = (neseq =>
        if (n < 0 || n >= length (s )
        ) NoElem [T]  ()
        else SomeElem [T]  (_atNonEmpty (neseq, n )  )      )    )

  def _atNonEmpty (xs: NESeq [T], n: Int ): T =
    {
      lazy val initial_value = AtFoldTuple [T]  (xs.head (), -1 )
      def next_value (tuple: AtFoldTuple [T], elem: T ): AtFoldTuple [T] = AtFoldTuple [T]  (elem, tuple.index + 1 )
      def condition (tuple: AtFoldTuple [T], elem: T ): Boolean = tuple.index < n
      foldLeftWhile (xs, initial_value, next_value, condition ) .elem }

  /* */

  def take (s: MSeq [T], n: Int ): MSeq [T] =
    {
      lazy val initial_value = TakeDropFoldTuple [T]  (empty, 0 )
      def next_value (tuple: TakeDropFoldTuple [T], elem: T ): TakeDropFoldTuple [T] =
        TakeDropFoldTuple [T]  (prepended (tuple.seq, elem ), tuple.index + 1 )
      def condition (tuple: TakeDropFoldTuple [T], elem: T ): Boolean = tuple.index < n
      reverse (foldLeftWhile (s, initial_value, next_value, condition ) .seq ) }

  def drop (s: MSeq [T], n: Int ): MSeq [T] =
    {
      lazy val initial_value = TakeDropFoldTuple [T]  (s, 0 )
      def next_value (tuple: TakeDropFoldTuple [T], elem: T ): TakeDropFoldTuple [T] =
        tuple.seq.opt (ifEmpty = TakeDropFoldTuple [T]  (tuple.seq, tuple.index + 1 ), ifNonEmpty = (neseq => TakeDropFoldTuple [T]  (neseq.tail (), tuple.index + 1 ) )        )
      def condition (tuple: TakeDropFoldTuple [T], elem: T ): Boolean =
        tuple.index < n
      foldLeftWhile (s, initial_value, next_value, condition ) .seq }

  def takeWhile (s: MSeq [T], p: (T => Boolean )  ): MSeq [T] = reverse (spanRevRec (s, p ) .left )

  def dropWhile (s: MSeq [T], p: (T => Boolean )  ): MSeq [T] = spanRevRec (s, p ) .right

  def splitAt (s: MSeq [T], n: Int ): MSeqPair [T] = MSeqPair (take (s, n ), drop (s, n )  )

  def span (s: MSeq [T], p: (T => Boolean )  ): MSeqPair [T] =
    {
      lazy val pair = spanRevRec (s, p )
      MSeqPair (reverse (pair.left ), pair.right ) }

  /* */

  def appended (s: MSeq [T], e: T ): MSeq [T] = reverse (prepended (reverse (s ), e )  )

  def last (s: NESeq [T]  ): T = reverseNonEmpty (s ) .head ()

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

  def exists (s: MSeq [T], p: (T => Boolean )  ): Boolean =
    {
      lazy val initial_value = false
      def next_value (acc: Boolean, elem: T ): Boolean = acc || p (elem )
      def condition (acc: Boolean, elem: T ): Boolean = ! acc
      foldLeftWhile (s, initial_value, next_value, condition ) }

  def find (s: MSeq [T], p: (T => Boolean )  ): OptionSD [T] =
    {
      lazy val initial_value = NoElem [T]  ()
      def next_value (acc: OptionSD [T], elem: T ): OptionSD [T] =
        if (p (elem ) ) SomeElem [T]  (elem ) else NoElem [T]  ()
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
  def foldLeft0 (mseq: MSeq [T]  ): (MSeq [T], ((MSeq [T], T ) => MSeq [T]  )  ) => MSeq [T] =
    (initial_value: MSeq [T], next_value: ((MSeq [T], T ) => MSeq [T]  )  ) => foldLeft (mseq, initial_value, next_value )

  /* */

  def spanRevRec (s0: MSeq [T], p: T => Boolean ): MSeqPair [T] =
    {
      lazy val result = MSeqPair (pair.right, pair.left )
      lazy val pair = foldLeftWhile (s0, initial_value, next_value, condition )

      lazy val initial_value = SpanRevFoldTuple [T]  (s0, empty, true )

      def next_value (tuple: SpanRevFoldTuple [T], elem: T ): SpanRevFoldTuple [T] =
        {
          lazy val left = tuple.left
          lazy val right = tuple.right
          lazy val result = left.opt (ifEmpty = SpanRevFoldTuple [T]  (left, right, false ), ifNonEmpty = (neleft =>
              {
                lazy val e = neleft.head ()
                lazy val new_taking = p (e )

                lazy val new_tuple =
                  if (new_taking
                  ) SpanRevFoldTuple [T]  (neleft.tail (), prepended (right, e ), new_taking )
                  else SpanRevFoldTuple [T]  (neleft, right, new_taking )
                new_tuple }            )          )
          result }

      def condition (tuple: SpanRevFoldTuple [T], elem: T ): Boolean = tuple.taking

      result }
}

case class MinImpl [T]  () extends Min [T]

case class IndexFoldTuple [T]  (index: Int, position: Int )

case class AtFoldTuple [T]  (elem: T, index: Int )

case class TakeDropFoldTuple [T]  (seq: MSeq [T], index: Int )

case class SpanRevFoldTuple [T]  (left: MSeq [T], right: MSeq [T], taking: Boolean )
