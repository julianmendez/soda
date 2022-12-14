package soda.collection

trait MSeqTranslator [A]
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_foldLeftSeq [B] (sequence : Seq [A] ) (current_value : B) (next_value : B => A => B) : B =
    if ( sequence.isEmpty
    ) current_value
    else _tailrec_foldLeftSeq (sequence.tail) (next_value (current_value) (sequence.head) ) (next_value)

  def foldLeftSeq [B] (sequence : Seq [A] ) (initial_value : B) (next_value : B => A => B) : B =
    _tailrec_foldLeftSeq (sequence) (initial_value) (next_value)

  def asMSeq (seq : Seq [A] ) : MSeq [A] =
    Min_ ().reverse (foldLeftSeq [MSeq [A] ] (seq) (_asMSeq_initial_value) (_asMSeq_next_value) )

  private lazy val _asMSeq_initial_value : MSeq [A] = Min_ ().empty

  private def _asMSeq_next_value (acc : MSeq [A] ) (elem : A) : MSeq [A] =
    Min_ ().prepended (acc) (elem)

  def asSeq (mseq : MSeq [A] ) : Seq [A] =
    (Min_ ().foldLeft (mseq) (_asSeq_initial_value) (_asSeq_next_value) ).reverse

  private lazy val _asSeq_initial_value : Seq [A] = Seq ()

  private def _asSeq_next_value (acc : Seq [A] ) (elem : A) : Seq [A] =
    acc.+: (elem)

}

case class MSeqTranslator_ [A] () extends MSeqTranslator [A]

trait MSeqPair [A]
{

  def   left : MSeq [A]
  def   right : MSeq [A]

}

case class MSeqPair_ [A] (left : MSeq [A], right : MSeq [A]) extends MSeqPair [A]

trait Min [A]
{

  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  lazy val empty : ESeq [A] = ESeq_ ()

  def prepended (s : MSeq [A] ) (e : A) : NESeq [A] =
    NESeq_ (e, s)

  def head (s : NESeq [A] ) : A =
    s.head

  def tail (s : NESeq [A] ) : MSeq [A] =
    s.tail

  def nonEmpty (s : MSeq [A] ) : Boolean =
    ! isEmpty (s)

  def isEmpty (s : MSeq [A] ) : Boolean =
    s.isEmpty

  /* */

  def foldLeftWhile [B] (s : MSeq [A] ) (initial_value : B) (next_value : B => A => B) (condition : B => A => Boolean) : B =
    MSeqRec_ [A] ().fold_while [B] (s) (initial_value) (next_value) (condition)

  def foldLeft [B] (s : MSeq [A] ) (initial_value : B) (next_value : B => A => B) : B =
    foldLeftWhile (s) (initial_value) (next_value) (  (acc : B) =>  (elem : A) => true)

  def reverse (s : MSeq [A] ) : MSeq [A] =
    s.opt [MSeq [A] ] (ifEmpty = empty) (ifNonEmpty =  neseq => reverseNonEmpty (neseq) )

  def reverseNonEmpty (s : NESeq [A] ) : NESeq [A] =
    foldLeft (s.tail) (_reverseNonEmpty_initial_value (s) ) (_reverseNonEmpty_next_value)

  private def _reverseNonEmpty_initial_value (s : NESeq [A] ) : NESeq [A] = prepended (empty) (s.head)

  private def _reverseNonEmpty_next_value (acc : MSeq [A] ) (elem : A) : NESeq [A] = prepended (acc) (elem)

  def length (s : MSeq [A] ) : Int =
    foldLeft (s) (_length_initial_value) (_length_next_value)

  private lazy val _length_initial_value : Int = 0

  private def _length_next_value (acc : Int) (elem : A) : Int =
    acc + 1

  def indexOf (s : MSeq [A] ) (e : A) : Int =
    (foldLeftWhile (s) (_indexOf_initial_value (e) ) (_indexOf_next_value) (_indexOf_condition) ).position

  private def _indexOf_initial_value (element : A) : IndexFoldTuple [A] = IndexFoldTuple_ [A] (0, -1, element)

  private def _indexOf_next_value (tuple : IndexFoldTuple [A] ) (current_element : A) : IndexFoldTuple [A] =
    IndexFoldTuple_ [A] (
      tuple.index + 1,
      ( if ( current_element == tuple.element
      ) tuple.index
      else tuple.position),
      tuple.element
    )

  private def _indexOf_condition (tuple : IndexFoldTuple [A] ) (elem : A) : Boolean =
    tuple.position == -1

  def contains (s : MSeq [A] ) (e : A) : Boolean =
    (foldLeftWhile (s) (_contains_initial_value (e) ) (_contains_next_value) (_contains_condition) ).contained

  private def _contains_initial_value (element : A) : ContainsFoldTuple [A] =
    ContainsFoldTuple_ (false, element)

  private def _contains_next_value (tuple : ContainsFoldTuple [A] ) (elem : A) : ContainsFoldTuple [A] =
    ContainsFoldTuple_ (elem == tuple.element, tuple.element)

  private def _contains_condition (tuple : ContainsFoldTuple [A] ) (elem : A) : Boolean =
    ! tuple.contained

  def at (s : MSeq [A] ) (n : Int) : OptionSD [A] =
    s.opt [OptionSD [A] ] (
      ifEmpty = NoneSD_ [A] () ) (
      ifNonEmpty = (  neseq =>
        if ( n < 0 || n >= length (s)
        ) NoneSD_ [A] ()
        else SomeSD_ [A] (_atNonEmpty (neseq ) (n) )
      )
    )

  private def _atNonEmpty (xs : NESeq [A] ) (n : Int) : A =
    (foldLeftWhile (xs) (_atNonEmpty_initial_value (xs) (n) ) (_atNonEmpty_next_value) (_atNonEmpty_condition) ).elem

  private def _atNonEmpty_initial_value (xs : NESeq [A] ) (position : Int ) : AtFoldTuple [A] =
    AtFoldTuple_ [A] (xs.head, -1, position)

  private def _atNonEmpty_next_value (tuple : AtFoldTuple [A] ) (elem : A) : AtFoldTuple [A] =
    AtFoldTuple_ [A] (elem, tuple.index + 1, tuple.position)

  private def _atNonEmpty_condition (tuple : AtFoldTuple [A] ) (elem : A) : Boolean =
    tuple.index < tuple.position

  /* */

  def take (s : MSeq [A] ) (n : Int) : MSeq [A] =
    reverse ( (foldLeftWhile (s) (_take_initial_value (n) ) (_take_next_value) (_take_condition) ).seq )

  private def _take_initial_value (length : Int ) : TakeDropFoldTuple [A] = TakeDropFoldTuple_ [A] (empty, 0, length)

  private def _take_next_value (tuple : TakeDropFoldTuple [A] ) (elem : A) : TakeDropFoldTuple [A] =
    TakeDropFoldTuple_ [A] (prepended (tuple.seq) (elem), tuple.index + 1, tuple.length)

  private def _take_condition (tuple : TakeDropFoldTuple [A] ) (elem : A) : Boolean = tuple.index < tuple.length

  def drop (s : MSeq [A] ) (n : Int) : MSeq [A] =
    (foldLeftWhile (s) (_drop_initial_value (s) (n) ) (_drop_next_value) (_drop_condition) ).seq

  private def _drop_initial_value (s : MSeq [A] ) (length : Int) : TakeDropFoldTuple [A] = TakeDropFoldTuple_ [A] (s, 0, length)

  private def _drop_next_value (tuple : TakeDropFoldTuple [A] ) (elem : A) : TakeDropFoldTuple [A] =
    tuple.seq.opt (
      ifEmpty = TakeDropFoldTuple_ [A] (tuple.seq, tuple.index + 1, tuple.length) ) (
      ifNonEmpty =  neseq => TakeDropFoldTuple_ [A] (neseq.tail, tuple.index + 1, tuple.length)
    )

  private def _drop_condition (tuple : TakeDropFoldTuple [A] ) (elem : A) : Boolean =
    tuple.index < tuple.length

  def takeWhile (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    reverse ( (spanRevRec (s) (p) ).left)

  def dropWhile (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    (spanRevRec (s) (p) ).right

  def splitAt (s : MSeq [A] ) (n : Int) : MSeqPair [A] =
    MSeqPair_ (take (s) (n), drop (s) (n) )

  def span (s : MSeq [A] ) (p : A => Boolean) : MSeqPair [A] =
    _span_with (s) (p) (spanRevRec (s) (p) )

  private def _span_with (s : MSeq [A] ) (p : A => Boolean) (pair : MSeqPair [A] ) : MSeqPair [A] =
    MSeqPair_ (reverse (pair.left), pair.right)

  /* */

  def appended (s : MSeq [A] ) (e : A) : MSeq [A] =
    reverse (prepended (reverse (s) ) (e) )

  def last (s : NESeq [A] ) : A =
    reverseNonEmpty (s).head

  def concat (s0 : MSeq [A] ) (s1 : MSeq [A] ) : MSeq [A] =
    foldLeft (reverse (s0) ) (_concat_initial_value (s1) ) (_concat_next_value)

  private def _concat_initial_value (s1 : MSeq [A] ) : MSeq [A] = s1

  private def _concat_next_value (acc : MSeq [A] ) (elem : A) : MSeq [A] = prepended (acc) (elem)

  def slice (s : MSeq [A] ) (from : Int) (until : Int) : MSeq [A] = take (drop (s) (from) ) (until - from)

  /* */

  def forall (s : MSeq [A] ) (p : A => Boolean) : Boolean =
    (foldLeftWhile (s) (_forall_initial_value (p) ) (_forall_next_value) (_forall_condition) ).status

  private def _forall_initial_value (condition : A => Boolean ) : ForallExistsFoldTuple [A] = ForallExistsFoldTuple_ (true, condition)

  private def _forall_next_value (tuple : ForallExistsFoldTuple [A] ) (elem : A) : ForallExistsFoldTuple [A] =
    ForallExistsFoldTuple_ (tuple.status && tuple.condition (elem), tuple.condition)

  private def _forall_condition (tuple : ForallExistsFoldTuple [A] ) (elem : A) : Boolean =
    tuple.status

  def exists (s : MSeq [A] ) (p : A => Boolean ) : Boolean =
    (foldLeftWhile (s) (_exists_initial_value (p) ) (_exists_next_value) (_exists_condition) ).status

  private def _exists_initial_value (condition : A => Boolean ) : ForallExistsFoldTuple [A] = ForallExistsFoldTuple_ (false, condition)

  private def _exists_next_value (tuple : ForallExistsFoldTuple [A] ) (elem : A) : ForallExistsFoldTuple [A] =
    ForallExistsFoldTuple_ (tuple.status || tuple.condition (elem), tuple.condition)

  private def _exists_condition (tuple : ForallExistsFoldTuple [A] ) (elem : A) : Boolean =
    ! tuple.status

  def find (s : MSeq [A] ) (p : A => Boolean) : OptionSD [A] =
    (foldLeftWhile (s) (_find_initial_value (p) ) (_find_next_value) (_find_condition) ).maybe_element

  private def _find_initial_value (condition : A => Boolean) : FindFoldTuple [A] =
    FindFoldTuple_ (NoneSD_ [A] (), condition)

  private def _find_next_value (tuple : FindFoldTuple [A] ) (elem : A) : FindFoldTuple [A] =
    if ( tuple.condition (elem)
    ) FindFoldTuple_ (SomeSD_ [A] (elem), tuple.condition)
    else FindFoldTuple_ (NoneSD_ [A] (), tuple.condition)

  private def _find_condition (tuple : FindFoldTuple [A] ) (elem : A) : Boolean =
    tuple.maybe_element.isEmpty

  def filter (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    reverse ( (foldLeft (s) (_filter_initial_value (p) ) (_filter_next_value) ).sequence )

  private def _filter_initial_value (condition : A => Boolean) : FilterFoldTuple [A] =
    FilterFoldTuple_ (empty, condition)

  private def _filter_next_value (tuple : FilterFoldTuple [A] ) (elem : A) :  FilterFoldTuple [A] =
    FilterFoldTuple_ (
      ( if ( tuple.condition (elem)
      ) prepended (tuple.sequence) (elem)
      else tuple.sequence),
      tuple.condition
    )

  def map0 (s : MSeq [A] ) (f : A => A) : MSeq [A] =
    reverse ( (foldLeft (s) (_map0_initial_value (f) ) (_map0_next_value) ).sequence)

  private def _map0_initial_value (mapping : A => A) : Map0FoldTuple [A] =
    Map0FoldTuple_ (empty, mapping)

  private def _map0_next_value (tuple : Map0FoldTuple [A] ) (elem : A) : Map0FoldTuple [A] =
    Map0FoldTuple_ (prepended (tuple.sequence) (tuple.mapping (elem) ), tuple.mapping)

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
  def foldLeft0 (mseq : MSeq [A] ) : MSeq [A] => (MSeq [A] => A => MSeq [A] ) => MSeq [A] =
     (initial_value : MSeq [A] ) =>
       (next_value : MSeq [A] => A => MSeq [A] ) =>
        foldLeft (mseq) (initial_value) (next_value)

  /* */

  def spanRevRec (s0 : MSeq [A] ) (p : A => Boolean) : MSeqPair [A] =
    _spanRevRec_for (foldLeftWhile (s0) (_spanRevRec_initial_value (s0) (p) ) (_spanRevRec_next_value) (_spanRevRec_condition) )

  private def _spanRevRec_for (tuple: SpanRevFoldTuple [A] ) : MSeqPair [A] =
    MSeqPair_ (tuple.right, tuple.left)

  private def _spanRevRec_initial_value (s0 : MSeq [A] ) (condition : A => Boolean) : SpanRevFoldTuple [A] =
    SpanRevFoldTuple_ [A] (s0, empty, true, condition)

  private def _spanRevRec_next_value (tuple : SpanRevFoldTuple [A] ) (elem : A) : SpanRevFoldTuple [A] =
    tuple.left.opt [SpanRevFoldTuple [A] ] (
      ifEmpty = SpanRevFoldTuple_ [A] (tuple.left, tuple.right, false, tuple.condition) ) (
      ifNonEmpty =  neleft => _aux_next_value (tuple) (neleft)
    )

  private def _spanRevRec_condition (tuple : SpanRevFoldTuple [A] ) (elem : A) : Boolean =
    tuple.taking

  private def _aux_next_value (tuple : SpanRevFoldTuple [A] ) (neleft : NESeq [A] ) : SpanRevFoldTuple [A] =
    _aux_next_value_for (tuple) (neleft) (tuple.condition (neleft.head) )

  private def _aux_next_value_for (tuple : SpanRevFoldTuple [A] ) (neleft : NESeq [A] ) (new_taking : Boolean) : SpanRevFoldTuple [A] =
    if ( new_taking
    ) SpanRevFoldTuple_ [A] (neleft.tail, prepended (tuple.right) (neleft.head), new_taking, tuple.condition)
    else SpanRevFoldTuple_ [A] (neleft, tuple.right, new_taking, tuple.condition)

}

case class Min_ [A] () extends Min [A]

trait IndexFoldTuple [A]
{

  def   index : Int
  def   position : Int
  def   element : A

}

case class IndexFoldTuple_ [A] (index : Int, position : Int, element : A) extends IndexFoldTuple [A]

trait ContainsFoldTuple [A]
{

  def   contained : Boolean
  def   element : A

}

case class ContainsFoldTuple_ [A] (contained : Boolean, element : A) extends ContainsFoldTuple [A]

trait AtFoldTuple [A]
{

  def   elem : A
  def   index : Int
  def   position : Int

}

case class AtFoldTuple_ [A] (elem : A, index : Int, position : Int) extends AtFoldTuple [A]

trait TakeDropFoldTuple [A]
{

  def   seq : MSeq [A]
  def   index : Int
  def   length : Int

}

case class TakeDropFoldTuple_ [A] (seq : MSeq [A], index : Int, length : Int) extends TakeDropFoldTuple [A]

trait ForallExistsFoldTuple [A]
{

  def   status : Boolean
  def   condition : A => Boolean

}

case class ForallExistsFoldTuple_ [A] (status : Boolean, condition : A => Boolean) extends ForallExistsFoldTuple [A]

trait FindFoldTuple [A]
{

  def   maybe_element : soda.lib.OptionSD [A]
  def   condition : A => Boolean

}

case class FindFoldTuple_ [A] (maybe_element : soda.lib.OptionSD [A], condition : A => Boolean) extends FindFoldTuple [A]

trait FilterFoldTuple [A]
{

  def   sequence : MSeq [A]
  def   condition : A => Boolean

}

case class FilterFoldTuple_ [A] (sequence : MSeq [A], condition : A => Boolean) extends FilterFoldTuple [A]

trait Map0FoldTuple [A]
{

  def   sequence : MSeq [A]
  def   mapping : A => A

}

case class Map0FoldTuple_ [A] (sequence : MSeq [A], mapping : A => A) extends Map0FoldTuple [A]

trait SpanRevFoldTuple [A]
{

  def   left : MSeq [A]
  def   right : MSeq [A]
  def   taking : Boolean
  def   condition : A => Boolean

}

case class SpanRevFoldTuple_ [A] (left : MSeq [A], right : MSeq [A], taking : Boolean, condition : A => Boolean) extends SpanRevFoldTuple [A]
