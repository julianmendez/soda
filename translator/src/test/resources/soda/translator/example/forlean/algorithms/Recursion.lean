/-
directive scala
object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}
-/

notation head "+:" tail => (head) :: (tail)
notation "Succ_" => Nat.succ
notation "Int" => Nat

/-
directive rocq
Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99).
Notation "'Succ_'" := S (at level 99).
Notation "'Int'" := nat (at level 99).
-/

/--
 This class contains tail recursive auxiliary functions.

-/

class FoldWhile

where
  mk ::
    
  deriving DecidableEq

namespace FoldWhile


private def   _tailrec_foldl_while ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
      (next : B -> A -> B) (condition : B -> A -> Bool) : B :=
    match sequence with
      | List.nil => current
      | (head) :: (tail) =>
        if (not (condition (current) (head) ) )
        then current
        else _tailrec_foldl_while ( A ) ( B ) (tail) (next (current) (head) ) (next) (condition)
    


def   apply ( A : Type ) ( B : Type ) (list : List ( A ) ) (initial : B)
      (next : B -> A -> B) (condition : B -> A -> Bool) : B :=
    _tailrec_foldl_while ( A ) ( B ) (list) (initial) (next) (condition)


end FoldWhile

notation "FoldWhile_" => FoldWhile.mk

class Fold

where
  mk ::
    
  deriving DecidableEq

namespace Fold


private def   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
      (next : B -> A -> B) : B :=
    match sequence with
      | List.nil => current
      | (head) :: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next (current) (head) ) (next)
    


def   apply ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial : B) (next : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (sequence) (initial) (next)


end Fold

notation "Fold_" => Fold.mk

class Range

where
  mk ::
    
  deriving DecidableEq

namespace Range


private def   _tailrec_range (non_negative_number : Int) (sequence : List ( Int ) ) : List ( Int ) :=
    match non_negative_number with
      | Succ_ (k) =>
        _tailrec_range (k) ( (k) :: (sequence) )
      | _otherwise => sequence
    


def   apply (length : Int) : List ( Int ) :=
    _tailrec_range (length) (List.nil)


end Range

notation "Range_" => Range.mk
