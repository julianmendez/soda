package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Coq.
 */





trait PairParam [A , B ]
{

  def   fst : A
  def   snd : B

}

case class PairParam_ [A, B] (fst : A, snd : B) extends PairParam [A, B]

object PairParam {
  def mk [A, B] (fst : A) (snd : B) : PairParam [A, B] =
    PairParam_ [A, B] (fst, snd)
}

trait PairParamMod
{



  def get_first [A , B ] (self : PairParam [A, B] ) : A =
    self .fst

  def get_second [A , B ] (self : PairParam [A, B] ) : B =
    self .snd

  def swap [A , B ] (self : PairParam [A, B] ) : PairParam [B, A] =
    PairParam_ (get_second [A, B] (self) , get_first [A, B] (self) )

}

case class PairParamMod_ () extends PairParamMod

object PairParamMod {
  def mk : PairParamMod =
    PairParamMod_ ()
}

trait TripleIntStringInt
  extends
    PairParam [Int, String]
{

  def   fst : Int
  def   snd : String
  def   trd : Int

}

case class TripleIntStringInt_ (fst : Int, snd : String, trd : Int) extends TripleIntStringInt

object TripleIntStringInt {
  def mk (fst : Int) (snd : String) (trd : Int) : TripleIntStringInt =
    TripleIntStringInt_ (fst, snd, trd)
}

trait TripleIntStringIntMod
{



  def get_first (self : TripleIntStringInt) : Int =
    self .fst

  def get_second (self : TripleIntStringInt) : String =
    self .snd

  def get_third (self : TripleIntStringInt) : Int =
    self .trd

  def get_pair_param (self : TripleIntStringInt) : PairParam [Int, String] =
    PairParam_ (get_first (self) , get_second (self) )

}

case class TripleIntStringIntMod_ () extends TripleIntStringIntMod

object TripleIntStringIntMod {
  def mk : TripleIntStringIntMod =
    TripleIntStringIntMod_ ()
}


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
      case Nil => current
      case (head) +: (tail) =>
        if ( (! (condition (current) (head) ) )
        ) current
        else _tailrec_foldl_while [A, B] (tail) (next (current) (head) ) (next) (condition)
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
      case Nil => current
      case (head) +: (tail) =>
        _tailrec_foldl [A, B] (tail) (next (current) (head) ) (next)
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


trait PairExample
{

  def   left : Int
  def   right : Int

}

case class PairExample_ (left : Int, right : Int) extends PairExample

object PairExample {
  def mk (left : Int) (right : Int) : PairExample =
    PairExample_ (left, right)
}

trait SwapExample
{



  def swap (pair : PairExample) : PairExample =
    pair match  {
      case PairExample_ (a, b) =>
        PairExample_ (b, a)
    }

/*
  directive lean
  theorem
    swap_of_swap (pair : PairExample)
      : (swap (swap (pair) ) ) = pair := by
    rewrite [swap, swap]
    simp
*/

}

case class SwapExample_ () extends SwapExample

object SwapExample {
  def mk : SwapExample =
    SwapExample_ ()
}

