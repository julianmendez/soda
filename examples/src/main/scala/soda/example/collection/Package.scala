package soda.collection

/*
 * This package contains examples for Soda.
 * They can be used as libraries, but their main purpose is
 * to show possible implementations of functions for list manipulation.
 */



trait MSeq [A ]
{

  def   isEmpty : Boolean

  def opt [B ] (ifEmpty : B) (ifNonEmpty : NESeq [A] => B) : B =
    this match  {
      case NESeq_ (head, tail) => ifNonEmpty (NESeq_ (head, tail) )
      case _otherwise => ifEmpty
    }

}

case class MSeq_ [A] (isEmpty : Boolean) extends MSeq [A]

object MSeq {
  def mk [A] (isEmpty : Boolean) : MSeq [A] =
    MSeq_ [A] (isEmpty)
}

trait ESeq [A ]
  extends
    MSeq [A]
{



  lazy val isEmpty = true

}

case class ESeq_ [A] () extends ESeq [A]

object ESeq {
  def mk [A] : ESeq [A] =
    ESeq_ [A] ()
}

trait NEMSeq [A ]
  extends
    MSeq [A]
{

  def   head0 : A
  def   tail0 : MSeq [A]

  lazy val isEmpty = false

}

case class NEMSeq_ [A] (head0 : A, tail0 : MSeq [A]) extends NEMSeq [A]

object NEMSeq {
  def mk [A] (head0 : A) (tail0 : MSeq [A]) : NEMSeq [A] =
    NEMSeq_ [A] (head0, tail0)
}

trait NESeq [A ]
  extends
    NEMSeq [A]
{

  def   head0 : A
  def   tail0 : MSeq [A]

  lazy val head : A = head0

  lazy val tail : MSeq [A] = tail0

}

case class NESeq_ [A] (head0 : A, tail0 : MSeq [A]) extends NESeq [A]

object NESeq {
  def mk [A] (head0 : A) (tail0 : MSeq [A]) : NESeq [A] =
    NESeq_ [A] (head0, tail0)
}

trait MSeqRec [A ]
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold_while [B ] (sequence : MSeq [A] ) (current_value : B)
      (next_value_function : B => A => B) (condition : B => A => Boolean) : B =
    sequence match  {
      case NESeq_ (head, tail) =>
        if ( ! condition (current_value) (head)
        ) current_value
        else _tailrec_fold_while (tail) (next_value_function (current_value) (head) ) (
          next_value_function) (condition)
      case _otherwise => current_value
    }

  def fold_while [B ] (sequence : MSeq [A] ) (initial_value : B) (next_value : B => A => B)
      (condition : B => A => Boolean) : B =
    _tailrec_fold_while (sequence) (initial_value) (next_value) (condition)

}

case class MSeqRec_ [A] () extends MSeqRec [A]

object MSeqRec {
  def mk [A] : MSeqRec [A] =
    MSeqRec_ [A] ()
}


trait IndexFoldTuple [A ]
{

  def   index : Int
  def   position : Int
  def   element : A

}

case class IndexFoldTuple_ [A] (index : Int, position : Int, element : A) extends IndexFoldTuple [A]

object IndexFoldTuple {
  def mk [A] (index : Int) (position : Int) (element : A) : IndexFoldTuple [A] =
    IndexFoldTuple_ [A] (index, position, element)
}

trait ContainsFoldTuple [A ]
{

  def   contained : Boolean
  def   element : A

}

case class ContainsFoldTuple_ [A] (contained : Boolean, element : A) extends ContainsFoldTuple [A]

object ContainsFoldTuple {
  def mk [A] (contained : Boolean) (element : A) : ContainsFoldTuple [A] =
    ContainsFoldTuple_ [A] (contained, element)
}

trait AtFoldTuple [A ]
{

  def   elem : A
  def   index : Int
  def   position : Int

}

case class AtFoldTuple_ [A] (elem : A, index : Int, position : Int) extends AtFoldTuple [A]

object AtFoldTuple {
  def mk [A] (elem : A) (index : Int) (position : Int) : AtFoldTuple [A] =
    AtFoldTuple_ [A] (elem, index, position)
}

trait TakeDropFoldTuple [A ]
{

  def   seq : MSeq [A]
  def   index : Int
  def   length : Int

}

case class TakeDropFoldTuple_ [A] (seq : MSeq [A], index : Int, length : Int) extends TakeDropFoldTuple [A]

object TakeDropFoldTuple {
  def mk [A] (seq : MSeq [A]) (index : Int) (length : Int) : TakeDropFoldTuple [A] =
    TakeDropFoldTuple_ [A] (seq, index, length)
}

trait ForallExistsFoldTuple [A ]
{

  def   status : Boolean
  def   condition : A => Boolean

}

case class ForallExistsFoldTuple_ [A] (status : Boolean, condition : A => Boolean) extends ForallExistsFoldTuple [A]

object ForallExistsFoldTuple {
  def mk [A] (status : Boolean) (condition : A => Boolean) : ForallExistsFoldTuple [A] =
    ForallExistsFoldTuple_ [A] (status, condition)
}

trait FindFoldTuple [A ]
{

  def   maybe_element : soda.lib.OptionSD [A]
  def   condition : A => Boolean

}

case class FindFoldTuple_ [A] (maybe_element : soda.lib.OptionSD [A], condition : A => Boolean) extends FindFoldTuple [A]

object FindFoldTuple {
  def mk [A] (maybe_element : soda.lib.OptionSD [A]) (condition : A => Boolean) : FindFoldTuple [A] =
    FindFoldTuple_ [A] (maybe_element, condition)
}

trait FilterFoldTuple [A ]
{

  def   sequence : MSeq [A]
  def   condition : A => Boolean

}

case class FilterFoldTuple_ [A] (sequence : MSeq [A], condition : A => Boolean) extends FilterFoldTuple [A]

object FilterFoldTuple {
  def mk [A] (sequence : MSeq [A]) (condition : A => Boolean) : FilterFoldTuple [A] =
    FilterFoldTuple_ [A] (sequence, condition)
}

trait Map0FoldTuple [A ]
{

  def   sequence : MSeq [A]
  def   mapping : A => A

}

case class Map0FoldTuple_ [A] (sequence : MSeq [A], mapping : A => A) extends Map0FoldTuple [A]

object Map0FoldTuple {
  def mk [A] (sequence : MSeq [A]) (mapping : A => A) : Map0FoldTuple [A] =
    Map0FoldTuple_ [A] (sequence, mapping)
}

trait SpanRevFoldTuple [A ]
{

  def   left : MSeq [A]
  def   right : MSeq [A]
  def   taking : Boolean
  def   condition : A => Boolean

}

case class SpanRevFoldTuple_ [A] (left : MSeq [A], right : MSeq [A], taking : Boolean, condition : A => Boolean) extends SpanRevFoldTuple [A]

object SpanRevFoldTuple {
  def mk [A] (left : MSeq [A]) (right : MSeq [A]) (taking : Boolean) (condition : A => Boolean) : SpanRevFoldTuple [A] =
    SpanRevFoldTuple_ [A] (left, right, taking, condition)
}

trait MSeqTranslator [A ]
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_foldLeftSeq [B ] (sequence : Seq [A] ) (current_value : B)
      (next_value : B => A => B) : B =
    if ( sequence .isEmpty
    ) current_value
    else _tailrec_foldLeftSeq (sequence .tail) (
      next_value (current_value) (sequence .head) ) (next_value)

  def foldLeftSeq [B ] (sequence : Seq [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_foldLeftSeq (sequence) (initial_value) (next_value)

  private lazy val _asMSeq_initial_value : MSeq [A] = Min_ () .empty

  private def _asMSeq_next_value (acc : MSeq [A] ) (elem : A) : MSeq [A] =
    Min_ () .prepended (acc) (elem)

  def asMSeq (seq : Seq [A] ) : MSeq [A] =
    Min_ () .reverse (
      foldLeftSeq [MSeq [A] ] (seq) (_asMSeq_initial_value) (_asMSeq_next_value) )

  private lazy val _asSeq_initial_value : Seq [A] = Seq ()

  private def _asSeq_next_value (acc : Seq [A] ) (elem : A) : Seq [A] =
    acc .+: (elem)

  def asSeq (mseq : MSeq [A] ) : Seq [A] =
    (Min_ () .foldLeft (mseq) (_asSeq_initial_value) (_asSeq_next_value) ) .reverse

}

case class MSeqTranslator_ [A] () extends MSeqTranslator [A]

object MSeqTranslator {
  def mk [A] : MSeqTranslator [A] =
    MSeqTranslator_ [A] ()
}

trait MSeqPair [A ]
{

  def   left : MSeq [A]
  def   right : MSeq [A]

}

case class MSeqPair_ [A] (left : MSeq [A], right : MSeq [A]) extends MSeqPair [A]

object MSeqPair {
  def mk [A] (left : MSeq [A]) (right : MSeq [A]) : MSeqPair [A] =
    MSeqPair_ [A] (left, right)
}

trait Min [A ]
{



  import   soda.lib.NoneSD_
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_

  lazy val empty : ESeq [A] = ESeq_ ()

  def prepended (s : MSeq [A] ) (e : A) : NESeq [A] =
    NESeq_ (e , s)

  def head (s : NESeq [A] ) : A =
    s .head

  def tail (s : NESeq [A] ) : MSeq [A] =
    s .tail

  def nonEmpty (s : MSeq [A] ) : Boolean =
    ! isEmpty (s)

  def isEmpty (s : MSeq [A] ) : Boolean =
    s .isEmpty

  /* */

  def foldLeftWhile [B ] (s : MSeq [A] ) (initial_value : B) (next_value : B => A => B)
      (condition : B => A => Boolean) : B =
    MSeqRec_ [A] () .fold_while [B] (s) (initial_value) (next_value) (condition)

  def foldLeft [B ] (s : MSeq [A] ) (initial_value : B) (next_value : B => A => B) : B =
    foldLeftWhile (s) (initial_value) (next_value) ( (acc : B) =>
       (elem : A) => true)

  private def _reverseNonEmpty_initial_value (s : NESeq [A] ) : NESeq [A] = prepended (empty) (s .head)

  private def _reverseNonEmpty_next_value (acc : MSeq [A] ) (elem : A) : NESeq [A] = prepended (acc) (elem)

  def reverseNonEmpty (s : NESeq [A] ) : NESeq [A] =
    foldLeft (s .tail) (_reverseNonEmpty_initial_value (s) ) (_reverseNonEmpty_next_value)

  def reverse (s : MSeq [A] ) : MSeq [A] =
    s match  {
      case NESeq_ (head, tail) => reverseNonEmpty (NESeq_ (head, tail) )
      case _otherwise => empty
    }

  private lazy val _length_initial_value : Int = 0

  private def _length_next_value (acc : Int) (elem : A) : Int =
    acc + 1

  def length (s : MSeq [A] ) : Int =
    foldLeft (s) (_length_initial_value) (_length_next_value)

  private def _mk_IndexFoldTuple [A ] (index : Int) (position : Int) (element : A)
      : IndexFoldTuple [A] =
    IndexFoldTuple_ (index, position, element)

  private def _indexOf_initial_value (element : A) : IndexFoldTuple [A] =
    _mk_IndexFoldTuple (0) (-1) (element)

  private def _get_index_if_element_found (tuple : IndexFoldTuple [A] ) (current_element : A) : Int =
    if ( current_element == tuple .element
    ) tuple .index
    else tuple .position

  private def _indexOf_next_value (tuple : IndexFoldTuple [A] ) (current_element : A) : IndexFoldTuple [A] =
    _mk_IndexFoldTuple (
      tuple .index + 1) (
      _get_index_if_element_found (tuple) (current_element) ) (
      tuple .element
    )

  private def _indexOf_condition (tuple : IndexFoldTuple [A] ) (elem : A) : Boolean =
    tuple .position == -1

  def indexOf (s : MSeq [A] ) (e : A) : Int =
    (foldLeftWhile (s) (_indexOf_initial_value (e) ) (_indexOf_next_value) (
      _indexOf_condition) ) .position

  private def _contains_initial_value (element : A) : ContainsFoldTuple [A] =
    ContainsFoldTuple_ (false, element)

  private def _contains_next_value (tuple : ContainsFoldTuple [A] ) (elem : A) : ContainsFoldTuple [A] =
    ContainsFoldTuple_ (elem == tuple .element, tuple .element)

  private def _contains_condition (tuple : ContainsFoldTuple [A] ) (elem : A) : Boolean =
    ! tuple .contained

  def contains (s : MSeq [A] ) (e : A) : Boolean =
    (foldLeftWhile (s) (_contains_initial_value (e) ) (_contains_next_value) (
      _contains_condition) ) .contained

  private def _atNonEmpty_initial_value (xs : NESeq [A] ) (position : Int ) : AtFoldTuple [A] =
    AtFoldTuple_ [A] (xs .head, -1, position)

  private def _atNonEmpty_next_value (tuple : AtFoldTuple [A] ) (elem : A) : AtFoldTuple [A] =
    AtFoldTuple_ [A] (elem, tuple .index + 1, tuple .position)

  private def _atNonEmpty_condition (tuple : AtFoldTuple [A] ) (elem : A) : Boolean =
    tuple .index < tuple .position

  private def _atNonEmpty (xs : NESeq [A] ) (n : Int) : A =
    (foldLeftWhile (xs) (_atNonEmpty_initial_value (xs) (n) ) (_atNonEmpty_next_value) (
      _atNonEmpty_condition) ) .elem

   private def _retrieve_if_in_range (xs : NESeq [A] ) (n : Int) : OptionSD [A] =
     if ( n < 0 || n >= length (xs)
     ) NoneSD_ [A] ()
     else SomeSD_ [A] (_atNonEmpty (xs) (n) )

  def at (s : MSeq [A] ) (n : Int) : OptionSD [A] =
    s match  {
      case NESeq_ (head, tail) => _retrieve_if_in_range (NESeq_ (head, tail) ) (n)
      case _otherwise => NoneSD_ [A] ()
    }

  /* */

  private def _mk_TakeDropFoldTuple [A ] (seq : MSeq [A] ) (index : Int) (length : Int)
      : TakeDropFoldTuple [A] =
    TakeDropFoldTuple_ (seq, index, length)

  private def _take_initial_value (length : Int ) : TakeDropFoldTuple [A] =
    _mk_TakeDropFoldTuple [A] (empty) (0) (length)

  private def _take_next_value (tuple : TakeDropFoldTuple [A] ) (elem : A) : TakeDropFoldTuple [A] =
    _mk_TakeDropFoldTuple [A] (prepended (tuple .seq) (elem) ) (tuple .index + 1) (
      tuple .length)

  private def _take_condition (tuple : TakeDropFoldTuple [A] ) (elem : A) : Boolean =
    tuple .index < tuple .length

  def take (s : MSeq [A] ) (n : Int) : MSeq [A] =
    reverse ( (foldLeftWhile (s) (_take_initial_value (n) ) (
      _take_next_value) (_take_condition) ) .seq )

  private def _drop_initial_value (s : MSeq [A] ) (length : Int) : TakeDropFoldTuple [A] =
    _mk_TakeDropFoldTuple [A] (s) (0) (length)

  private def _drop_next_value (tuple : TakeDropFoldTuple [A] ) (elem : A) : TakeDropFoldTuple [A] =
    (tuple .seq) match  {
      case NESeq_ (head, tail) => _mk_TakeDropFoldTuple [A] (tail) (tuple .index + 1) (
        tuple .length)
      case _otherwise => _mk_TakeDropFoldTuple [A] (tuple .seq) (tuple .index + 1) (
        tuple .length)
    }

  private def _drop_condition (tuple : TakeDropFoldTuple [A] ) (elem : A) : Boolean =
    tuple .index < tuple .length

  def drop (s : MSeq [A] ) (n : Int) : MSeq [A] =
    (foldLeftWhile (s) (_drop_initial_value (s) (n) ) (_drop_next_value) (_drop_condition) )
      .seq

  /* */

  private def _mk_SpanRevFoldTuple [A ] (left : MSeq [A] ) (right : MSeq [A] ) (taking : Boolean)
      (condition : A => Boolean) : SpanRevFoldTuple [A] =
    SpanRevFoldTuple_ (left, right, taking, condition)

  private def _spanRevRec_initial_value (s0 : MSeq [A] ) (condition : A => Boolean) : SpanRevFoldTuple [A] =
    _mk_SpanRevFoldTuple [A] (s0) (empty) (true) (condition)

  private def _aux_next_value_for (tuple : SpanRevFoldTuple [A] ) (neleft : NESeq [A] )
      (new_taking : Boolean) : SpanRevFoldTuple [A] =
    if ( new_taking
    ) _mk_SpanRevFoldTuple [A] (neleft .tail) (prepended (tuple .right) (neleft .head) ) (
      new_taking) (tuple .condition)
    else _mk_SpanRevFoldTuple [A] (neleft) (tuple .right) (new_taking) (tuple .condition)

  private def _aux_next_value (tuple : SpanRevFoldTuple [A] ) (neleft : NESeq [A] ) : SpanRevFoldTuple [A] =
    _aux_next_value_for (tuple) (neleft) (tuple .condition (neleft .head) )

  private def _spanRevRec_next_value (tuple : SpanRevFoldTuple [A] ) (elem : A) : SpanRevFoldTuple [A] =
    (tuple .left) match  {
      case NESeq_ (head, tail) => _aux_next_value (tuple) (NESeq_ (head, tail) )
      case _otherwise =>
        _mk_SpanRevFoldTuple [A] (tuple .left) (tuple .right) (false) (tuple .condition)
    }

  private def _spanRevRec_condition (tuple : SpanRevFoldTuple [A] ) (elem : A) : Boolean =
    tuple .taking

  private def _spanRevRec_for (tuple: SpanRevFoldTuple [A] ) : MSeqPair [A] =
    MSeqPair_ (tuple .right, tuple .left)

  def spanRevRec (s0 : MSeq [A] ) (p : A => Boolean) : MSeqPair [A] =
    _spanRevRec_for (foldLeftWhile (s0) (_spanRevRec_initial_value (s0) (p) ) (
      _spanRevRec_next_value) (_spanRevRec_condition) )

  def takeWhile (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    reverse ( (spanRevRec (s) (p) ) .left)

  def dropWhile (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    (spanRevRec (s) (p) ) .right

  def splitAt (s : MSeq [A] ) (n : Int) : MSeqPair [A] =
    MSeqPair_ (take (s) (n) , drop (s) (n) )

  private def _span_with (s : MSeq [A] ) (p : A => Boolean) (pair : MSeqPair [A] ) : MSeqPair [A] =
    MSeqPair_ (reverse (pair .left) , pair .right)

  def span (s : MSeq [A] ) (p : A => Boolean) : MSeqPair [A] =
    _span_with (s) (p) (spanRevRec (s) (p) )

  /* */

  def appended (s : MSeq [A] ) (e : A) : MSeq [A] =
    reverse (prepended (reverse (s) ) (e) )

  def last (s : NESeq [A] ) : A =
    reverseNonEmpty (s) .head

  private def _concat_initial_value (s1 : MSeq [A] ) : MSeq [A] = s1

  private def _concat_next_value (acc : MSeq [A] ) (elem : A) : MSeq [A] = prepended (acc) (elem)

  def concat (s0 : MSeq [A] ) (s1 : MSeq [A] ) : MSeq [A] =
    foldLeft (reverse (s0) ) (_concat_initial_value (s1) ) (_concat_next_value)

  def slice (s : MSeq [A] ) (from : Int) (until : Int) : MSeq [A] =
    take (drop (s) (from) ) (until - from)

  /* */

  private def _mk_ForallExistsFoldTuple [A ] (status : Boolean) (condition : A => Boolean)
      : ForallExistsFoldTuple [A] =
    ForallExistsFoldTuple_ (status, condition)

  private def _forall_initial_value (condition : A => Boolean ) : ForallExistsFoldTuple [A] =
    _mk_ForallExistsFoldTuple (true) (condition)

  private def _forall_next_value (tuple : ForallExistsFoldTuple [A] ) (elem : A)
      : ForallExistsFoldTuple [A] =
    _mk_ForallExistsFoldTuple (tuple .status && tuple .condition (elem) ) (tuple .condition)

  private def _forall_condition (tuple : ForallExistsFoldTuple [A] ) (elem : A) : Boolean =
    tuple .status

  def forall (s : MSeq [A] ) (p : A => Boolean) : Boolean =
    (foldLeftWhile (s) (_forall_initial_value (p) ) (_forall_next_value) (_forall_condition) )
      .status

  private def _exists_initial_value (condition : A => Boolean ) : ForallExistsFoldTuple [A] =
    _mk_ForallExistsFoldTuple (false) (condition)

  private def _exists_next_value (tuple : ForallExistsFoldTuple [A] ) (elem : A)
      : ForallExistsFoldTuple [A] =
    _mk_ForallExistsFoldTuple (tuple .status || tuple .condition (elem) ) (tuple .condition)

  private def _exists_condition (tuple : ForallExistsFoldTuple [A] ) (elem : A) : Boolean =
    ! tuple .status

  def exists (s : MSeq [A] ) (p : A => Boolean ) : Boolean =
    (foldLeftWhile (s) (_exists_initial_value (p) ) (_exists_next_value) (_exists_condition) )
      .status

  private def _mk_FindFoldTuple [A ] (maybe_element : soda.lib.OptionSD [A] ) (
      condition : A => Boolean) : FindFoldTuple [A] =
    FindFoldTuple_ (maybe_element, condition)

  private def _find_initial_value (condition : A => Boolean) : FindFoldTuple [A] =
    _mk_FindFoldTuple (NoneSD_ [A] () ) (condition)

  private def _find_next_value (tuple : FindFoldTuple [A] ) (elem : A) : FindFoldTuple [A] =
    if ( tuple .condition (elem)
    ) _mk_FindFoldTuple (SomeSD_ [A] (elem) ) (tuple .condition)
    else _mk_FindFoldTuple (NoneSD_ [A] () ) (tuple .condition)

  private def _find_condition (tuple : FindFoldTuple [A] ) (elem : A) : Boolean =
    tuple .maybe_element .isEmpty

  def find (s : MSeq [A] ) (p : A => Boolean) : OptionSD [A] =
    (foldLeftWhile (s) (_find_initial_value (p) ) (_find_next_value) (_find_condition) )
      .maybe_element

  private def _filter_initial_value (condition : A => Boolean) : FilterFoldTuple [A] =
    FilterFoldTuple_ (empty, condition)

  private def _prepend_when_condition (tuple : FilterFoldTuple [A] ) (elem : A) :  MSeq [A] =
    if ( tuple .condition (elem)
    ) prepended (tuple .sequence) (elem)
    else tuple .sequence

  private def _filter_next_value (tuple : FilterFoldTuple [A] ) (elem : A) :  FilterFoldTuple [A] =
    FilterFoldTuple_ (_prepend_when_condition (tuple) (elem) , tuple .condition)

  def filter (s : MSeq [A] ) (p : A => Boolean) : MSeq [A] =
    reverse ( (foldLeft (s) (_filter_initial_value (p) ) (_filter_next_value) ) .sequence )

  private def _map0_initial_value (mapping : A => A) : Map0FoldTuple [A] =
    Map0FoldTuple_ (empty, mapping)

  private def _map0_next_value (tuple : Map0FoldTuple [A] ) (elem : A) : Map0FoldTuple [A] =
    Map0FoldTuple_ (prepended (tuple .sequence) (tuple .mapping (elem) ) , tuple .mapping)

  def map0 (s : MSeq [A] ) (f : A => A) : MSeq [A] =
    reverse ( (foldLeft (s) (_map0_initial_value (f) ) (_map0_next_value) ) .sequence)

  /**
   * <pre>
   * def foldLeft [B] (z : B) (op : (B , A) -> B) : B =
   * . var result = z
   * . it = iterator
   * . while (it.hasNext) {
   * . . result = op (result , it.next () )
   * . }
   * . result
   * end
   * </pre>
   */
  def foldLeft0 (mseq : MSeq [A] ) : MSeq [A] => (MSeq [A] => A => MSeq [A] ) => MSeq [A] =
     (initial_value : MSeq [A] ) =>
       (next_value : MSeq [A] => A => MSeq [A] ) =>
        foldLeft (mseq) (initial_value) (next_value)

}

case class Min_ [A] () extends Min [A]

object Min {
  def mk [A] : Min [A] =
    Min_ [A] ()
}

