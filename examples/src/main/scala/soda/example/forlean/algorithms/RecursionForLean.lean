notation:max "Nil" => List.nil

notation:max "Boolean" => Bool

notation:max "Zero_ ()" => Nat.zero

notation:max "Succ_" => Nat.succ

/- This class contains tail recursive auxiliary functions.
-/

namespace RecursionForLean

def   _tailrec_fold4 ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) (condition : B -> A -> Boolean) : B :=
    match sequence with
      | (head) :: (tail) =>
         if (not (condition (current) (head) ) )
         then current
         else
           _tailrec_fold4 ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value) (condition)
      | otherwise => current
    


def   fold4 ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) (condition : B -> A -> Boolean) : B :=
    _tailrec_fold4 ( A ) ( B ) (sequence) (initial_value) (next_value) (condition)


def   _tailrec_fold3 ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) : B :=
    match sequence with
      | (head) :: (tail) =>
        _tailrec_fold3 ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value)
      | otherwise => current
    


def   fold3 ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) : B :=
    _tailrec_fold3 ( A ) ( B ) (sequence) (initial_value) (next_value)


 def   _tailrec_range (n : Nat) (sequence : List ( Nat ) ) : List ( Nat ) :=
    match n with
      | Zero_ () => sequence
      | Succ_ (k) => _tailrec_range (k) (k :: sequence)
    


 def   range (length : Nat) : List ( Nat ) :=
    _tailrec_range (length) (Nil)


end RecursionForLean

open RecursionForLean
