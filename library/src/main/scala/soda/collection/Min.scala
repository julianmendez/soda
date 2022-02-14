package soda.collection

trait MSeqTranslator [A]
{

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_foldLeftSeq [B] (sequence: Seq [A] ) (current_value: B ) (next_value: B => A => B ): B =
    if (sequence.isEmpty
    ) current_value
    else _tailrec_foldLeftSeq (sequence.tail ) (next_value (current_value ) (sequence.head ) ) (next_value )

  def foldLeftSeq [B] (sequence: Seq [A] ) (initial_value: B ) (next_value: B => A => B ): B =
    _tailrec_foldLeftSeq (sequence ) (initial_value ) (next_value )

  def asMSeq (seq: Seq [A] ): MSeq [A] =
    {
      lazy val initial_value: MSeq [A] = Min_ () .empty
      def next_value (acc: MSeq [A] ) (elem: A ): MSeq [A] = Min_ () .prepended (acc ) (elem )
      Min_ () .reverse (foldLeftSeq [MSeq [A]] (seq ) (initial_value ) (next_value ) ) }

  def asSeq (mseq: MSeq [A] ): Seq [A] =
    {
      lazy val initial_value: Seq [A] = Seq ()
      def next_value (acc: Seq [A] ) (elem: A ): Seq [A] = acc.+: (elem )
      (Min_ () .foldLeft (mseq ) (initial_value ) (next_value ) ) .reverse }

}

case class MSeqTranslator_ [A] () extends MSeqTranslator [A]

trait MSeqPair [A]
{

  def   left: MSeq [A]
  def   right: MSeq [A]

}

case class MSeqPair_ [A] (left: MSeq [A], right: MSeq [A]) extends MSeqPair [A]

trait Min [A]
{

  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  lazy val empty: ESeq [A] = ESeq_ ()

  def prepended (s: MSeq [A] ) (e: A ): NESeq [A] =
    NESeq_ (e, s )

  def head (s: NESeq [A] ): A =
    s.head

  def tail (s: NESeq [A] ): MSeq [A] =
    s.tail

  def nonEmpty (s: MSeq [A] ): Boolean =
    ! isEmpty (s )

  def isEmpty (s: MSeq [A] ): Boolean =
    s.isEmpty

  /* */

  def foldLeftWhile [B] (s: MSeq [A] ) (initial_value: B ) (next_value: B => A => B ) (condition: B => A => Boolean ): B =
    MSeqRec_ [A] () .fold_while [B] (s ) (initial_value ) (next_value ) (condition )

  def foldLeft [B] (s: MSeq [A] ) (initial_value: B ) (next_value: B => A => B ): B =
    foldLeftWhile (s ) (initial_value ) (next_value ) ((acc: B ) =>  (elem: A ) => true )

  def reverse (s: MSeq [A] ): MSeq [A] =
    s.opt [MSeq [A]] (ifEmpty = empty ) (ifNonEmpty = neseq => reverseNonEmpty (neseq ) )

  def reverseNonEmpty (s: NESeq [A] ): NESeq [A] =
    {
      lazy val initial_value: NESeq [A] = prepended (empty ) (s.head )
      def next_value (acc: MSeq [A] ) (elem: A ): NESeq [A] = prepended (acc ) (elem )
      foldLeft (s.tail ) (initial_value ) (next_value ) }

  def length (s: MSeq [A] ): Int =
    {
      lazy val initial_value: Int = 0
      def next_value (acc: Int ) (elem: A ): Int = acc + 1
      foldLeft (s ) (initial_value ) (next_value ) }

  def indexOf (s: MSeq [A] ) (e: A ): Int =
    {
      lazy val initial_value: IndexFoldTuple [A] = IndexFoldTuple_ [A] (0, -1 )
      def next_value (tuple: IndexFoldTuple [A] ) (elem: A ): IndexFoldTuple [A] =
        IndexFoldTuple_ [A] (tuple.index + 1,
          if (elem == e ) tuple.index else tuple.position )
      def condition (tuple: IndexFoldTuple [A] ) (elem: A ): Boolean = tuple.position == -1
      (foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) ) .position }

  def contains (s: MSeq [A] ) (e: A ): Boolean =
    {
      lazy val initial_value: Boolean = false
      def next_value (acc: Boolean ) (elem: A ): Boolean = elem == e
      def condition (acc: Boolean ) (elem: A ): Boolean = ! acc
      foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) }

  def at (s: MSeq [A] ) (n: Int ): OptionSD [A] =
    s.opt [OptionSD [A]] (
      ifEmpty = NoneSD_ [A] () ) (
      ifNonEmpty = (neseq =>
        if (n < 0 || n >= length (s )
        ) NoneSD_ [A] ()
        else SomeSD_ [A] (_atNonEmpty (neseq ) (n ) )
      )
    )

  def _atNonEmpty (xs: NESeq [A] ) (n: Int ): A =
    {
      lazy val initial_value: AtFoldTuple [A] = AtFoldTuple_ [A] (xs.head, -1 )
      def next_value (tuple: AtFoldTuple [A] ) (elem: A ): AtFoldTuple [A] = AtFoldTuple_ [A] (elem, tuple.index + 1 )
      def condition (tuple: AtFoldTuple [A] ) (elem: A ): Boolean = tuple.index < n
      (foldLeftWhile (xs ) (initial_value ) (next_value ) (condition ) ) .elem }

  /* */

  def take (s: MSeq [A] ) (n: Int ): MSeq [A] =
    {
      lazy val initial_value: TakeDropFoldTuple [A] = TakeDropFoldTuple_ [A] (empty, 0 )
      def next_value (tuple: TakeDropFoldTuple [A] ) (elem: A ): TakeDropFoldTuple [A] =
        TakeDropFoldTuple_ [A] (prepended (tuple.seq ) (elem ), tuple.index + 1 )
      def condition (tuple: TakeDropFoldTuple [A] ) (elem: A ): Boolean = tuple.index < n
      reverse ((foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) ) .seq ) }

  def drop (s: MSeq [A] ) (n: Int ): MSeq [A] =
    {
      lazy val initial_value: TakeDropFoldTuple [A] = TakeDropFoldTuple_ [A] (s, 0 )
      def next_value (tuple: TakeDropFoldTuple [A] ) (elem: A ): TakeDropFoldTuple [A] =
        tuple.seq.opt (
          ifEmpty = TakeDropFoldTuple_ [A] (tuple.seq, tuple.index + 1 ) ) (
          ifNonEmpty = neseq => TakeDropFoldTuple_ [A] (neseq.tail, tuple.index + 1 )
        )
      def condition (tuple: TakeDropFoldTuple [A] ) (elem: A ): Boolean =
        tuple.index < n
      (foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) ) .seq }

  def takeWhile (s: MSeq [A] ) (p: A => Boolean ): MSeq [A] = reverse ((spanRevRec (s ) (p ) ) .left )

  def dropWhile (s: MSeq [A] ) (p: A => Boolean ): MSeq [A] = (spanRevRec (s ) (p ) ) .right

  def splitAt (s: MSeq [A] ) (n: Int ): MSeqPair [A] = MSeqPair_ (take (s ) (n ), drop (s ) (n ) )

  def span (s: MSeq [A] ) (p: A => Boolean ): MSeqPair [A] =
    {
      lazy val pair = spanRevRec (s ) (p )
      MSeqPair_ (reverse (pair.left ), pair.right ) }

  /* */

  def appended (s: MSeq [A] ) (e: A ): MSeq [A] = reverse (prepended (reverse (s ) ) (e ) )

  def last (s: NESeq [A] ): A = reverseNonEmpty (s ) .head

  def concat (s0: MSeq [A] ) (s1: MSeq [A] ): MSeq [A] =
    {
      lazy val initial_value: MSeq [A] = s1
      def next_value (acc: MSeq [A] ) (elem: A ): MSeq [A] = prepended (acc ) (elem )
      lazy val s0rev = reverse (s0 )
      foldLeft (s0rev ) (initial_value ) (next_value ) }

  def slice (s: MSeq [A] ) (from: Int ) (until: Int ): MSeq [A] = take (drop (s ) (from ) ) (until - from )

  /* */

  def forall (s: MSeq [A] ) (p: A => Boolean ): Boolean =
    {
      lazy val initial_value: Boolean = true
      def next_value (acc: Boolean ) (elem: A ): Boolean = acc && p (elem )
      def condition (acc: Boolean ) (elem: A ): Boolean = acc
      foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) }

  def exists (s: MSeq [A] ) (p: A => Boolean ): Boolean =
    {
      lazy val initial_value: Boolean = false
      def next_value (acc: Boolean ) (elem: A ): Boolean = acc || p (elem )
      def condition (acc: Boolean ) (elem: A ): Boolean = ! acc
      foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) }

  def find (s: MSeq [A] ) (p: A => Boolean ): OptionSD [A] =
    {
      lazy val initial_value: OptionSD [A] = NoneSD_ [A] ()
      def next_value (acc: OptionSD [A] ) (elem: A ): OptionSD [A] =
        if (p (elem ) ) SomeSD_ [A] (elem ) else NoneSD_ [A] ()
      def condition (acc: OptionSD [A] ) (elem: A ): Boolean = acc.isEmpty
      foldLeftWhile (s ) (initial_value ) (next_value ) (condition ) }

  def filter (s: MSeq [A] ) (p: A => Boolean ): MSeq [A] =
    {
      lazy val initial_value: MSeq [A] = empty
      def next_value (acc: MSeq [A] ) (elem: A ): MSeq [A] =
        if (p (elem )
        ) prepended (acc ) (elem )
        else acc
      reverse (foldLeft (s ) (initial_value ) (next_value ) ) }

  def map0 (s: MSeq [A] ) (f: A => A ): MSeq [A] =
    {
      lazy val initial_value: MSeq [A] = empty
      def next_value (acc: MSeq [A] ) (elem: A ): MSeq [A] =
        prepended (acc ) (f (elem ) )
      reverse (foldLeft (s ) (initial_value ) (next_value ) ) }

  /**
   * <pre>
   * def foldLeft [B] (z : B) (op : (B, A) -> B) : B =
   * . var result = z
   * . it = iterator
   * . while (it.hasNext) {
   * . . result = op (result, it.next () )
   * . }
   * . result
   * end
   * </pre>
   */
  def foldLeft0 (mseq: MSeq [A] ): MSeq [A] => (MSeq [A] => A => MSeq [A] ) => MSeq [A] =
     (initial_value: MSeq [A] ) =>
       (next_value: MSeq [A] => A => MSeq [A] ) =>
        foldLeft (mseq ) (initial_value ) (next_value )

  /* */

  def spanRevRec (s0: MSeq [A] ) (p: A => Boolean ): MSeqPair [A] =
    {
      lazy val result = MSeqPair_ (pair.right, pair.left )
      lazy val pair = foldLeftWhile (s0 ) (initial_value ) (next_value ) (condition )
      lazy val initial_value: SpanRevFoldTuple [A] = SpanRevFoldTuple_ [A] (s0, empty, true )
      def next_value (tuple: SpanRevFoldTuple [A] ) (elem: A ): SpanRevFoldTuple [A] =
        {
          lazy val left = tuple.left
          lazy val right = tuple.right
          lazy val result = left.opt [SpanRevFoldTuple [A]] (
            ifEmpty = SpanRevFoldTuple_ [A] (left, right, false ) ) (
            ifNonEmpty = neleft => _aux_next_value (tuple ) (p ) (neleft )
          )
          result }
      def condition (tuple: SpanRevFoldTuple [A] ) (elem: A ): Boolean = tuple.taking
      result }

  def _aux_next_value (tuple: SpanRevFoldTuple [A] ) (p: A => Boolean ) (neleft: NESeq [A] ): SpanRevFoldTuple [A] =
    {
      lazy val left = tuple.left
      lazy val right = tuple.right
      lazy val e = neleft.head
      lazy val new_taking = p (e )
      lazy val new_tuple =
        if (new_taking
        ) SpanRevFoldTuple_ [A] (neleft.tail, prepended (right ) (e ), new_taking )
        else SpanRevFoldTuple_ [A] (neleft, right, new_taking )
      new_tuple }

}

case class Min_ [A] () extends Min [A]

trait IndexFoldTuple [A]
{

  def   index: Int
  def   position: Int

}

case class IndexFoldTuple_ [A] (index: Int, position: Int) extends IndexFoldTuple [A]

trait AtFoldTuple [A]
{

  def   elem: A
  def   index: Int

}

case class AtFoldTuple_ [A] (elem: A, index: Int) extends AtFoldTuple [A]

trait TakeDropFoldTuple [A]
{

  def   seq: MSeq [A]
  def   index: Int

}

case class TakeDropFoldTuple_ [A] (seq: MSeq [A], index: Int) extends TakeDropFoldTuple [A]

trait SpanRevFoldTuple [A]
{

  def   left: MSeq [A]
  def   right: MSeq [A]
  def   taking: Boolean

}

case class SpanRevFoldTuple_ [A] (left: MSeq [A], right: MSeq [A], taking: Boolean) extends SpanRevFoldTuple [A]
