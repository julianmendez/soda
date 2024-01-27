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
 * This class contains auxiliary functions for combinations.
 */

trait CartesianProduct
{



  lazy val fold = Fold_ ()

  private def _initial [A ] (seq : Seq [A] ) : Seq [Seq [A] ] =
    seq .map ( elem => (elem) +: Nil)

  private def _next [A ] (accum : Seq [Seq [A] ] ) (seq : Seq [A] ) : Seq [Seq [A] ] =
    seq .flatMap ( elem =>
      accum .map ( seq_b => (elem) +: (seq_b) ) )

  private def _apply_recursion [A ] (rev_sequences : Seq [Seq [A] ] ) : Seq [Seq [A] ] =
    fold (rev_sequences .tail) (_initial [A] (rev_sequences .head) ) (_next [A] )

  def apply [A ] (sequences : Seq [Seq [A] ] ) : Seq [Seq [A] ] =
    if ( sequences .isEmpty
    ) sequences
    else _apply_recursion [A] (sequences .reverse)

}

case class CartesianProduct_ () extends CartesianProduct

object CartesianProduct {
  def mk : CartesianProduct =
    CartesianProduct_ ()
}


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

object Doc {
  def mk (doc : String) : Doc =
    Doc_ (doc)
}


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

object EnumConstant {
  def mk (ordinal : Int) (name : String) : EnumConstant =
    EnumConstant_ (ordinal, name)
}

trait Enum [A <: EnumConstant]
{

  def   values : Seq [A]

}

case class Enum_ [A <: EnumConstant] (values : Seq [A]) extends Enum [A]

object Enum {
  def mk [A <: EnumConstant] (values : Seq [A]) : Enum [A] =
    Enum_ [A] (values)
}


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This is an Option implemented without exceptions.
 */

trait OptionSD [A ]
{

  def   toOption : Option [A]
  def   isEmpty : Boolean
  def   isDefined : Boolean
  def   isNonEmpty : Boolean
  def   toSeq : Seq [A]

  def opt [B ] (ifEmpty : B) (ifNonEmpty : A => B) : B =
    this match  {
      case SomeSD_ (element) => ifNonEmpty (element)
      case otherwise => ifEmpty
    }

  def map [B ] (mapping : A => B) : OptionSD [B] =
    this match  {
      case SomeSD_ (element) => SomeSD_ [B] (mapping (element) )
      case otherwise => NoneSD_ [B] ()
    }

  def getOrElse (default : A) : A =
    this match  {
      case SomeSD_ (element) => element
      case otherwise => default
    }

  def fold [B ] (ifEmpty : B) (f : A => B) : B =
    this match  {
      case SomeSD_ (element) => f (element)
      case otherwise => ifEmpty
    }

  def flatMap [B ] (mapping : A => OptionSD [B] ) : OptionSD [B] =
    this match  {
      case SomeSD_ (element) => mapping (element)
      case otherwise => NoneSD_ [B] ()
    }

  def bind [B ] (mapping : A => OptionSD [B] ) : OptionSD [B] =
    flatMap [B] (mapping)

  def filter (predicate : A => Boolean) : OptionSD [A] =
    this match  {
      case SomeSD_ (element) =>
        if ( predicate (element)
        ) this
        else NoneSD_ [A] ()
      case otherwise => NoneSD_ [A] ()
    }

}

case class OptionSD_ [A] (toOption : Option [A], isEmpty : Boolean, isDefined : Boolean, isNonEmpty : Boolean, toSeq : Seq [A]) extends OptionSD [A]

object OptionSD {
  def mk [A] (toOption : Option [A]) (isEmpty : Boolean) (isDefined : Boolean) (isNonEmpty : Boolean) (toSeq : Seq [A]) : OptionSD [A] =
    OptionSD_ [A] (toOption, isEmpty, isDefined, isNonEmpty, toSeq)
}

trait NoneSD [A ]
  extends
    OptionSD [A]
{



  lazy val toOption : None .type = None

  lazy val isEmpty : Boolean = true

  lazy val isDefined : Boolean = ! isEmpty

  lazy val isNonEmpty : Boolean = ! isEmpty

  lazy val toSeq : Seq [A] = Seq [A] ()

}

case class NoneSD_ [A] () extends NoneSD [A]

object NoneSD {
  def mk [A] : NoneSD [A] =
    NoneSD_ [A] ()
}

trait OptionSDWithElement [A ]
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

object OptionSDWithElement {
  def mk [A] (toOption : Option [A]) (isEmpty : Boolean) (isDefined : Boolean) (isNonEmpty : Boolean) (toSeq : Seq [A]) (element : A) : OptionSDWithElement [A] =
    OptionSDWithElement_ [A] (toOption, isEmpty, isDefined, isNonEmpty, toSeq, element)
}

trait SomeSD [A ]
  extends
    OptionSDWithElement [A]
{

  def   element : A

  lazy val value : A = element

  lazy val toOption : Some [A] = Some [A] (element)

  lazy val isEmpty : Boolean = false

  lazy val isDefined : Boolean = ! isEmpty

  lazy val isNonEmpty : Boolean = ! isEmpty

  lazy val toSeq : Seq [A] = Seq [A] (element)

}

case class SomeSD_ [A] (element : A) extends SomeSD [A]

object SomeSD {
  def mk [A] (element : A) : SomeSD [A] =
    SomeSD_ [A] (element)
}

trait OptionSDBuilder [A ]
{



  def build (option : Option [A] ) : OptionSD [A] =
    option match  {
      case Some (content) => SomeSD_ [A] (content)
      case otherwise => NoneSD_ [A] ()
    }

}

case class OptionSDBuilder_ [A] () extends OptionSDBuilder [A]

object OptionSDBuilder {
  def mk [A] : OptionSDBuilder [A] =
    OptionSDBuilder_ [A] ()
}


/*
 * This file is automatically generated. Do not edit.
 */

object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}

/*
directive lean
notation head "+:" tail => (head) :: (tail)
notation "Succ_" => Nat.succ
notation "Int" => Nat
*/

/*
directive coq
Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99) .
Notation "'Succ_'" := S (at level 99) .
Notation "'Int'" := nat (at level 99) .
*/

/**
 * This class contains tail recursive auxiliary functions.
 */

trait FoldWhile
{



  private def _tailrec_foldl_while [A , B ] (sequence : Seq [A] ) (current : B)
      (next : B => A => B) (condition : B => A => Boolean) : B =
    sequence match  {
      case (head) +: (tail) =>
        if ( (! (condition (current) (head) ) )
        ) current
        else _tailrec_foldl_while [A, B] (tail) (next (current) (head) ) (next) (condition)
      case _otherwise => current
    }

  def apply [A , B ] (list : Seq [A] ) (initial : B)
      (next : B => A => B) (condition : B => A => Boolean) : B =
    _tailrec_foldl_while [A, B] (list) (initial) (next) (condition)

}

case class FoldWhile_ () extends FoldWhile

object FoldWhile {
  def mk : FoldWhile =
    FoldWhile_ ()
}

trait Fold
{



  private def _tailrec_foldl [A , B ] (sequence : Seq [A] ) (current : B)
      (next : B => A => B) : B =
    sequence match  {
      case (head) +: (tail) =>
        _tailrec_foldl [A, B] (tail) (next (current) (head) ) (next)
      case _otherwise => current
    }

  def apply [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B) : B =
    _tailrec_foldl [A, B] (sequence) (initial) (next)

}

case class Fold_ () extends Fold

object Fold {
  def mk : Fold =
    Fold_ ()
}

trait Range
{



  private def _tailrec_range (non_negative_number : Int) (sequence : Seq [Int] ) : Seq [Int] =
    non_negative_number match  {
      case Succ_ (k) =>
        _tailrec_range (k) ( (k) +: (sequence) )
      case _otherwise => sequence
    }

  def apply (length : Int) : Seq [Int] =
    _tailrec_range (length) (Nil)

}

case class Range_ () extends Range

object Range {
  def mk : Range =
    Range_ ()
}


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This is a Seq implemented without exceptions.
 */

trait SeqSD [A ]
{

  def   toSeq : Seq [A]
  def   reverse : SeqSD [A]

  def opt [B ] (ifEmpty : B) (ifNonEmpty : NonEmptySeqSD [A] => B) : B =
    this match  {
      case NonEmptySeqSD_ (toSeq) => ifNonEmpty (NonEmptySeqSD_ [A] (toSeq) )
      case otherwise => ifEmpty
    }

}

case class SeqSD_ [A] (toSeq : Seq [A], reverse : SeqSD [A]) extends SeqSD [A]

object SeqSD {
  def mk [A] (toSeq : Seq [A]) (reverse : SeqSD [A]) : SeqSD [A] =
    SeqSD_ [A] (toSeq, reverse)
}

trait EmptySeqSD [A ]
  extends
    SeqSD [A]
{



  lazy val toSeq : Seq [A] = Seq [A] ()

  lazy val reverse : EmptySeqSD [A] = this

}

case class EmptySeqSD_ [A] () extends EmptySeqSD [A]

object EmptySeqSD {
  def mk [A] : EmptySeqSD [A] =
    EmptySeqSD_ [A] ()
}

trait NonEmptySeqSD [A ]
  extends
    SeqSD [A]
{

  def   toSeq : Seq [A]

  lazy val head : A = toSeq .head

  lazy val tail : SeqSD [A] = SeqSDBuilder_ [A] () .build (toSeq .tail)

  lazy val reverse : NonEmptySeqSD [A] = NonEmptySeqSD_ [A] (toSeq .reverse)

}

case class NonEmptySeqSD_ [A] (toSeq : Seq [A]) extends NonEmptySeqSD [A]

object NonEmptySeqSD {
  def mk [A] (toSeq : Seq [A]) : NonEmptySeqSD [A] =
    NonEmptySeqSD_ [A] (toSeq)
}

trait SeqSDBuilder [A ]
{



  def build (seq : Seq [A] ) : SeqSD [A] =
    if ( seq .isEmpty
    ) EmptySeqSD_ [A] ()
    else NonEmptySeqSD_ [A] (seq)

}

case class SeqSDBuilder_ [A] () extends SeqSDBuilder [A]

object SeqSDBuilder {
  def mk [A] : SeqSDBuilder [A] =
    SeqSDBuilder_ [A] ()
}


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This class models an exception in Soda.
 */

trait SodaException [A ]
  extends
    java.lang.Throwable
{

   def   message : String
   def   cause : A

}

case class SodaException_ [A] (message : String, cause : A) extends SodaException [A]

object SodaException {
  def mk [A] (message : String) (cause : A) : SodaException [A] =
    SodaException_ [A] (message, cause)
}


/* Soda library */

