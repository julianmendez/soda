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

