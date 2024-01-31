(*
directive scala
object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}
*)

(*
directive lean
notation head "+:" tail => (head) :: (tail)
notation "Succ_" => Nat.succ
notation "Int" => Nat
*)

Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99) .
Notation "'Succ_'" := S (at level 99) .
Notation "'Int'" := nat (at level 99) .

(** This class contains tail recursive auxiliary functions.
*)

Module FoldWhile .

Class FoldWhile : Type :=

  mk {
    
} .

Notation "'FoldWhile_'" := FoldWhile.mk (at level 99) .

Fixpoint   _tailrec_foldl_while ( A : Type ) ( B : Type ) (sequence : list ( A ) ) (current : B)
       (next : B -> A -> B) (condition : B -> A -> bool) : B :=
    match sequence with
      | nil => current
      | (head) +: (tail) =>
        if (negb (condition (current) (head) ) )
        then current
        else _tailrec_foldl_while ( A ) ( B ) (tail) (next (current) (head) ) (next) (condition)
    end
.

Definition   apply ( A : Type ) ( B : Type ) (list : list ( A ) ) (initial : B)
       (next : B -> A -> B) (condition : B -> A -> bool) : B :=
    _tailrec_foldl_while ( A ) ( B ) (list) (initial) (next) (condition)
.

End FoldWhile .

Import FoldWhile .

Module Fold .

Class Fold : Type :=

  mk {
    
} .

Notation "'Fold_'" := Fold.mk (at level 99) .

Fixpoint   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : list ( A ) ) (current : B)
       (next : B -> A -> B) : B :=
    match sequence with
      | nil => current
      | (head) +: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next (current) (head) ) (next)
    end
.

 Definition   apply ( A : Type ) ( B : Type ) (sequence : list ( A ) ) (initial : B) (next : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (sequence) (initial) (next)
.

End Fold .

Import Fold .

Module Range .

Class Range : Type :=

  mk {
    
} .

Notation "'Range_'" := Range.mk (at level 99) .

 Fixpoint   _tailrec_range (non_negative_number : Int) (sequence : list ( Int ) ) : list ( Int ) :=
    match non_negative_number with
      | Succ_ (k) =>
        _tailrec_range (k) ( (k) +: (sequence) )
      | _otherwise => sequence
    end
.

 Definition   apply (length : Int) : list ( Int ) :=
    _tailrec_range (length) (nil)
.

End Range .

Import Range .
