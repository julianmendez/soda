package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Coq.
 */

import   soda.example.forlean.lib.Nat
import   soda.example.forlean.lib.Succ_
import   soda.example.forlean.lib.Zero_

trait Package

/*
directive lean
/- Prelude for Soda types. -/
notation "Boolean" => Bool
notation "None" => Option.none
notation "Some" => Option.some
notation "Nil" => List.nil
notation "Zero_ ()" => Nat.zero
notation "Succ_ " =>  Nat.succ
*/

trait MyList
{



/*
 * foldl
 * (fold left)
 */

  private def _tailrec_foldl [A , B ] (sequence : List [A] ) (current : B)
      (next_value : B => A => B) : B =
    sequence match  {
      case Nil => current
      case (head) :: (tail) =>
        _tailrec_foldl [A, B] (tail) (next_value (current) (head) ) (next_value)
    }

  def foldl [A , B ] (sequence : List [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_foldl [A, B] (sequence) (initial_value) (next_value)

/*
 * length
 */

  def length_fl [A ] (list : List [A] ) : Nat =
    foldl [A, Nat] (list) (Zero_ () ) (
       (accum : Nat) =>
         (elem : A) => Succ_ (accum)
    )

/*
  directive lean
  theorem
    len_fl_accum (A : Type) (list : List (A) )
       : forall (accum : Nat) ,
        _tailrec_foldl (A) (Nat) (list) (accum) (
          fun (accum : Nat) => fun (elem : A) => accum + 1) =
        _tailrec_foldl (A) (Nat) (list) (0) (
          fun (accum : Nat) => fun (elem : A) => accum + 1) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        simp
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        have h1 := by exact ih (1)
        have h2 := by exact ih (n + 1)
        rewrite [h1]
        rewrite [h2]
        simp [Nat.add_assoc, Nat.add_comm]
*/

  private def _tailrec_length [A ] (list : List [A] ) (accum : Nat) : Nat =
    list match  {
      case Nil => accum
      case (head) :: (tail) =>
        _tailrec_length [A] (tail) (Succ_ (accum) )
    }

/*
  directive lean
  theorem
    len_tr_accum (A : Type) (list : List (A) )
      : forall (accum : Nat) ,
        _tailrec_length (A) (list) (accum)  = _tailrec_length (A) (list) (0) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        simp
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        have h1 := by exact ih (1)
        have h2 := by exact ih (n + 1)
        rewrite [h1]
        rewrite [h2]
        simp [Nat.add_assoc, Nat.add_comm]
*/

  def length_tr [A ] (list : List [A] ) : Nat =
    _tailrec_length [A] (list) (Zero_ () )

  def length_def [A ] (list : List [A] ) : Nat =
    list match  {
      case Nil => Zero_ ()
      case (head) :: (tail) => Succ_ (length_def [A] (tail) )
    }

/*  theorem
    len_fl_eq_len_def (A : Type) (list : List (A))
      : length_fl (A) (list) = length_def (A) (list) := by
    rewrite [length_fl, foldl]
    induction list with
    | nil =>
      rewrite [_tailrec_foldl, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_foldl, len_fl_accum]
      rewrite [ih]
      rewrite [length_def]
      simp
*/

/*
  directive lean
  theorem
    len_tr_eq_len_def
      : length_tr = length_def := by
    funext A list
    rewrite [length_tr]
    induction list with
    | nil =>
      constructor
    | cons head tail ih =>
      rewrite [_tailrec_length, len_tr_accum]
      rewrite [ih]
      rewrite [length_def]
      simp
*/

  def length [A ] (list : List [A] ) : Nat =
    length_fl [A] (list)

}

case class MyList_ () extends MyList


trait PairParam [A , B ]
{

  def   fst : A
  def   snd : B

}

case class PairParam_ [A, B] (fst : A, snd : B) extends PairParam [A, B]

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

trait TripleIntStringInt
  extends
    PairParam [Int, String]
{

  def   fst : Int
  def   snd : String
  def   trd : Int

}

case class TripleIntStringInt_ (fst : Int, snd : String, trd : Int) extends TripleIntStringInt

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


/*
directive lean
notation:max "Nil" => List.nil
notation:max "Boolean" => Bool
notation:max "Zero_ ()" => Nat.zero
notation:max "Succ_" => Nat.succ
*/

/**
 * This class contains tail recursive auxiliary functions.
 */

trait RecursionForLean
{



  private def _tailrec_fold4 [A , B ] (list : List [A] ) (current : B)
      (next : B => A => B) (condition : B => A => Boolean) : B =
    list match  {
      case Nil => current
      case (head) :: (tail) =>
        if ( (! (condition (current) (head) ) )
        ) current
        else _tailrec_fold4 [A, B] (tail) (next (current) (head) ) (next) (condition)
    }

  def fold4 [A , B ] (list : List [A] ) (initial_value : B)
      (next : B => A => B) (condition : B => A => Boolean) : B =
    _tailrec_fold4 [A, B] (list) (initial_value) (next) (condition)

  private def _tailrec_fold3 [A , B ] (list : List [A] ) (current : B)
      (next : B => A => B) : B =
    list match  {
      case Nil => current
      case (head) :: (tail) =>
        _tailrec_fold3 [A, B] (tail) (next (current) (head) ) (next)
    }

  def fold3 [A , B ] (list : List [A] ) (initial_value : B)
      (next : B => A => B) : B =
    _tailrec_fold3 [A, B] (list) (initial_value) (next)

  private def _tailrec_range (n : Nat) (list : List [Nat] ) : List [Nat] =
    n match  {
      case Zero_ () => list
      case Succ_ (k) =>
        _tailrec_range (k) ( (k) :: (list) )
    }

  def range (length : Nat) : List [Nat] =
    _tailrec_range (length) (Nil)

}

case class RecursionForLean_ () extends RecursionForLean


trait PairExample
{

  def   left : Int
  def   right : Int

}

case class PairExample_ (left : Int, right : Int) extends PairExample

trait SwapExample
{



  def swap (pair : PairExample) : PairExample =
    PairExample_ (pair .right, pair .left)

/*
  directive lean
  theorem
    swap_of_swap (x : Int) (y : Int) : (swap (swap (PairExample_ (x, y) ) ) ) = PairExample_ (x, y) :=
      by
        constructor
*/

}

case class SwapExample_ () extends SwapExample

