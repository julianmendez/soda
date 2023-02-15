/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/*
 * This package contains a small collection of classes that are useful in Soda.
 * These classes can be imported instead of the Scala or Java classes,
 * or can be used as an inspiration for similar developments.
 */



trait Package
/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This is a Seq implemented without exceptions.
 */

trait SeqSD [A]
{

  def   toSeq : Seq [A]
  def   reverse : SeqSD [A]

  def opt [B] (ifEmpty : B) (ifNonEmpty : NonEmptySeqSD [A] => B) : B =
    this match  {
      case NonEmptySeqSD_ (toSeq) => ifNonEmpty (NonEmptySeqSD_ (toSeq) )
      case x => ifEmpty
    }

}

case class SeqSD_ [A] (toSeq : Seq [A], reverse : SeqSD [A]) extends SeqSD [A]

trait EmptySeqSD [A]
  extends
    SeqSD [A]
{

  lazy val toSeq : Seq [A] = Seq [A] ()

  lazy val reverse : EmptySeqSD [A] = this

}

case class EmptySeqSD_ [A] () extends EmptySeqSD [A]

trait NonEmptySeqSD [A]
  extends
    SeqSD [A]
{

  def   toSeq : Seq [A]

  lazy val head : A = toSeq.head

  lazy val tail : SeqSD [A] = SeqSDBuilder_ [A] ().build (toSeq.tail)

  lazy val reverse : NonEmptySeqSD [A] = NonEmptySeqSD_ (toSeq.reverse)

}

case class NonEmptySeqSD_ [A] (toSeq : Seq [A]) extends NonEmptySeqSD [A]

trait SeqSDBuilder [A]
{

  def build (seq : Seq [A] ) : SeqSD [A] =
    if ( seq.isEmpty
    ) EmptySeqSD_ [A] ()
    else NonEmptySeqSD_ [A] (seq)

}

case class SeqSDBuilder_ [A] () extends SeqSDBuilder [A]


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This class contains tail recursive auxiliary functions.
 */

trait FoldWhile
{

  def apply [A, B]
    (sequence : Seq [A] )
    (initial_value : B)
    (next_value_function : B => A => B)
    (condition : B => A => Boolean)
      : B =
    _tailrec_fold_while (sequence) (initial_value) (next_value_function) (condition)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold_while [A, B] (sequence : Seq [A] ) (current_value : B) (next_value_function : B => A => B) (condition : B => A => Boolean) : B =
    if ( sequence.isEmpty || ( ! condition (current_value) (sequence.head) )
    ) current_value
    else _tailrec_fold_while (sequence.tail) (next_value_function (current_value) (sequence.head) ) (next_value_function) (condition)

}

case class FoldWhile_ () extends FoldWhile

trait Fold
{

  def apply [A, B]
    (sequence : Seq [A] )
    (initial_value : B)
    (next_value_function : B => A => B)
      : B =
    _tailrec_fold (sequence) (initial_value) (next_value_function)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold [A, B] (sequence : Seq [A] ) (current_value : B) (next_value_function : B => A => B) : B =
    if ( sequence.isEmpty
    ) current_value
    else _tailrec_fold (sequence.tail) (next_value_function (current_value) (sequence.head) ) (next_value_function)

}

case class Fold_ () extends Fold

trait Range
{

  def apply (length : Int) : Seq [Int] =
    _tailrec_range (length) (Seq [Int] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_range (n : Int) (sequence : Seq [Int] ) : Seq [Int] =
    if ( n <= 0
    ) sequence
    else _tailrec_range (n - 1) (sequence.+: (n - 1) )

}

case class Range_ () extends Range

trait Recursion
{

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  def fold_while [A, B] (sequence : Seq [A] ) (initial_value : B) (next_value_function : B => A => B) (condition : B => A => Boolean) : B =
    _fold_while.apply (sequence) (initial_value) (next_value_function) (condition)

  def fold [A, B] (sequence : Seq [A] ) (initial_value : B) (next_value_function : B => A => B) : B =
    _fold.apply (sequence) (initial_value) (next_value_function)

  def range (length : Int) : Seq [Int] =
    _range.apply (length)

}

case class Recursion_ () extends Recursion


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This is a constant to be used in enumerations.
 */

trait EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class EnumConstant_ (ordinal : Int, name : String) extends EnumConstant

trait Enum [A <: EnumConstant]
{

  def   values : Seq [A]

}

case class Enum_ [A <: EnumConstant] (values : Seq [A]) extends Enum [A]


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This class contains auxiliary functions for combinations.
 */

trait CartesianProduct
{

  def apply [A] (sequences : Seq [Seq [A] ] ) : Seq [Seq [A] ] =
    if ( sequences.isEmpty
    ) sequences
    else _apply_recursion (sequences.reverse)

  private def _apply_recursion [A] (rev_sequences : Seq [Seq [A] ] ) : Seq [Seq [A] ] =
    _fold.apply (rev_sequences.tail) (_initial_value (rev_sequences.head) ) (_next_value [A] )

  private lazy val _fold = Fold_ ()

  private def _initial_value [A] (seq : Seq [A] ) : Seq [Seq [A] ] =
    seq.map (  elem => Seq [A] (elem) )

  private def _next_value [A] (accum : Seq [Seq [A] ] ) (seq_a : Seq [A] ) : Seq [Seq [A] ] =
    seq_a.flatMap (  elem_a =>
      accum.map (  seq_b => seq_b.+: (elem_a) ) )

}

case class CartesianProduct_ () extends CartesianProduct


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This is an Option implemented without exceptions.
 */

trait OptionSD [A]
{

  def   toOption : Option [A]
  def   isEmpty : Boolean
  def   isDefined : Boolean
  def   isNonEmpty : Boolean
  def   toSeq : Seq [A]

  def opt [B] (ifEmpty : B) (ifNonEmpty : A => B) : B =
    this match  {
      case SomeSD_ (element) => ifNonEmpty (element)
      case x => ifEmpty
    }

  def map [B] (mapping : A => B) : OptionSD [B] =
    this match  {
      case SomeSD_ (element) => SomeSD_ [B] (mapping (element) )
      case x => NoneSD_ [B] ()
    }

  def getOrElse (default : A) : A =
    this match  {
      case SomeSD_ (element) => element
      case x => default
    }

  def fold [B] (ifEmpty : B) (f : A => B) : B =
    this match  {
      case SomeSD_ (element) => f (element)
      case x => ifEmpty
    }

  def flatMap [B] (mapping : A => OptionSD [B] ) : OptionSD [B] =
    this match  {
      case SomeSD_ (element) => mapping (element)
      case x => NoneSD_ ()
    }

  def bind [B] (mapping : A => OptionSD [B] ) : OptionSD [B] =
    flatMap [B] (mapping)

  def filter (predicate : A => Boolean) : OptionSD [A] =
    this match  {
      case SomeSD_ (element) =>
        if ( predicate (element)
        ) this
        else NoneSD_ [A] ()
      case x => NoneSD_ ()
    }

}

case class OptionSD_ [A] (toOption : Option [A], isEmpty : Boolean, isDefined : Boolean, isNonEmpty : Boolean, toSeq : Seq [A]) extends OptionSD [A]

trait NoneSD [A]
  extends
    OptionSD [A]
{

  lazy val toOption : None.type = None

  lazy val isEmpty : Boolean = true

  lazy val isDefined : Boolean = ! isEmpty

  lazy val isNonEmpty : Boolean = ! isEmpty

  lazy val toSeq : Seq [A] = Seq ()

}

case class NoneSD_ [A] () extends NoneSD [A]

trait OptionSDWithElement [A]
  extends
    OptionSD [A]
{

  def   toOption : Option [A]
  def   isEmpty : Boolean
  def   isDefined : Boolean
  def   isNonEmpty : Boolean
  def   toSeq : Seq [A]
  def   element : A

}

case class OptionSDWithElement_ [A] (toOption : Option [A], isEmpty : Boolean, isDefined : Boolean, isNonEmpty : Boolean, toSeq : Seq [A], element : A) extends OptionSDWithElement [A]

trait SomeSD [A]
  extends
    OptionSDWithElement [A]
{

  def   element : A

  lazy val value : A = element

  lazy val toOption : Some [A] = Some [A] (element)

  lazy val isEmpty : Boolean = false

  lazy val isDefined : Boolean = ! isEmpty

  lazy val isNonEmpty : Boolean = ! isEmpty

  lazy val toSeq : Seq [A] = Seq (element)

}

case class SomeSD_ [A] (element : A) extends SomeSD [A]

trait OptionSDBuilder [A]
{

  def build (option : Option [A] ) : OptionSD [A] =
    if ( option.isEmpty
    ) NoneSD_ [A] ()
    else SomeSD_ [A] (option.get)

}

case class OptionSDBuilder_ [A] () extends OptionSDBuilder [A]


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * All concrete classes extending this class need to have a 'doc' attribute.
 */

trait Doc
{

  def   doc : String

}

case class Doc_ (doc : String) extends Doc


