notation:max "Nil" => List.nil
notation:max "Boolean" => Bool
notation:max "Zero_ ()" => Nat.zero
notation:max "Succ_" => Nat.succ

/-- This class contains tail recursive auxiliary functions.
-/

class RecursionForLean

where
  RecursionForLean_ ::
    
  deriving DecidableEq

namespace RecursionForLean


private def   _tailrec_fold4 ( A : Type ) ( B : Type ) (list : List ( A ) ) (current : B)
       (next : B -> A -> B) (condition : B -> A -> Boolean) : B :=
    match list with
      | [] => current
      | (head) :: (tail) =>
        if (not (condition (current) (head) ) )
        then current
        else _tailrec_fold4 ( A ) ( B ) (tail) (next (current) (head) ) (next) (condition)
    


def   fold4 ( A : Type ) ( B : Type ) (list : List ( A ) ) (initial_value : B)
       (next : B -> A -> B) (condition : B -> A -> Boolean) : B :=
    _tailrec_fold4 ( A ) ( B ) (list) (initial_value) (next) (condition)


private def   _tailrec_fold3 ( A : Type ) ( B : Type ) (list : List ( A ) ) (current : B)
       (next : B -> A -> B) : B :=
    match list with
      | [] => current
      | (head) :: (tail) =>
        _tailrec_fold3 ( A ) ( B ) (tail) (next (current) (head) ) (next)
    


def   fold3 ( A : Type ) ( B : Type ) (list : List ( A ) ) (initial_value : B)
       (next : B -> A -> B) : B :=
    _tailrec_fold3 ( A ) ( B ) (list) (initial_value) (next)


 private def   _tailrec_range (n : Nat) (list : List ( Nat ) ) : List ( Nat ) :=
    match n with
      | Zero_ () => list
      | Succ_ (k) =>
        _tailrec_range (k) ( (k) :: (list) )
    


 def   range (length : Nat) : List ( Nat ) :=
    _tailrec_range (length) (Nil)


end RecursionForLean

open RecursionForLean
