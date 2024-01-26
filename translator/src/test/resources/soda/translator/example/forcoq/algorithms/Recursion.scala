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
Require Import Coq.Init.Nat .
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
      case 0 => sequence
      case Succ_ (k) =>
        _tailrec_range (k) ( (k) +: (sequence) )
    }

  def apply (length : Int) : Seq [Int] =
    if ( length < 0
    ) Nil
    else _tailrec_range (length) (Nil)

}

case class Range_ () extends Range

object Range {
  def mk : Range =
    Range_ ()
}
