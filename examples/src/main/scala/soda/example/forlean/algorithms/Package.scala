package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Coq.
 */

import   soda.example.forlean.lib.Nat
import   soda.example.forlean.lib.Succ_
import   soda.example.forlean.lib.Zero_

trait Package

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
*/

/*
directive lean
notation:max "Boolean" => Bool
*/

/*
directive lean
notation:max "Zero_ ()" => Nat.zero
*/

/*
directive lean
notation:max "Succ_" => Nat.succ
*/

/*
 * This class contains tail recursive auxiliary functions.
 */

trait RecursionForLean
{

  private def _tailrec_fold4 [A , B ] (sequence : List [A] ) (current : B)
      (next_value : B => A => B) (condition : B => A => Boolean) : B =
    sequence match  {
      case (head) :: (tail) =>
         if ( (! (condition (current) (head) ) )
         ) current
         else
           _tailrec_fold4 [A, B] (tail) (next_value (current) (head) ) (next_value) (condition)
      case otherwise => current
    }

  def fold4 [A , B ] (sequence : List [A] ) (initial_value : B)
      (next_value : B => A => B) (condition : B => A => Boolean) : B =
    _tailrec_fold4 [A, B] (sequence) (initial_value) (next_value) (condition)

  private def _tailrec_fold3 [A , B ] (sequence : List [A] ) (current : B)
      (next_value : B => A => B) : B =
    sequence match  {
      case (head) :: (tail) =>
        _tailrec_fold3 [A, B] (tail) (next_value (current) (head) ) (next_value)
      case otherwise => current
    }

  def fold3 [A , B ] (sequence : List [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_fold3 [A, B] (sequence) (initial_value) (next_value)

  private def _tailrec_range (n : Nat) (sequence : List [Nat] ) : List [Nat] =
    n match  {
      case Zero_ () => sequence
      case Succ_ (k) => _tailrec_range (k) (k :: sequence)
    }

  def range (length : Nat) : List [Nat] =
    _tailrec_range (length) (Nil)

}

case class RecursionForLean_ () extends RecursionForLean


/*
directive coq
Definition Nat : Type := nat .
*/

trait PairExample
{

  def   left : Nat
  def   right : Nat

}

case class PairExample_ (left : Nat, right : Nat) extends PairExample

trait SwapExample
{

  def swap (pair : PairExample) : PairExample =
    PairExample_ (pair .right, pair .left)

/*
  directive lean
  theorem
    swap_of_swap (x : Nat) (y : Nat) : (swap (swap (PairExample_ (x, y) ) ) ) = PairExample_ (x, y) :=
      by
        constructor
*/

/*
  directive coq
  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x, y) ) ) ) =
    PairExample_ (x, y) .
  Proof.
    auto.
  Qed.
*/

}

case class SwapExample_ () extends SwapExample

