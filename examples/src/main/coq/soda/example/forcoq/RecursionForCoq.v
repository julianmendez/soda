(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Definition Boolean := bool .

Definition Seq (A: Type) := list A .

Module soda_example_forcoq.

Module RecursionForCoq.

  Fixpoint _tailrec_fold4 {A B: Type} (sequence: Seq (A)) (current_value: B) (next_value_function: B -> A -> B) (condition: B -> A -> Boolean): B :=
    match sequence with
      | nil => current_value
      | (head) :: (tail) =>
        if (negb (condition (current_value) (head) ) )
        then current_value
        else _tailrec_fold4 (tail) (next_value_function (current_value) (head)) (next_value_function) (condition)
    end .

  Definition fold4 {A B: Type} (sequence: Seq (A)) (initial_value: B) (next_value_function: B -> A -> B) (condition: B -> A -> Boolean): B :=
    _tailrec_fold4 (sequence) (initial_value) (next_value_function) (condition) .

  Fixpoint _rec_fold3 {A B: Type} (sequence: Seq (A)) (current_value: B) (next_value_function: B -> A -> B): B :=
    match sequence with
      | nil => current_value
      | (head) :: (tail) => _rec_fold3 (tail) (next_value_function (current_value) (head)) (next_value_function)
    end .

  Definition fold3 {A B: Type} (sequence: Seq (A)) (initial_value: B) (next_value_function: B -> A -> B): B :=
    _rec_fold3 (sequence) (initial_value) (next_value_function) .

  Fixpoint _rec_range (n: nat) (sequence: Seq (nat) ): Seq (nat) :=
    match n with
      | O => sequence
      | S (k) => _rec_range (k) ( (k) :: sequence )
    end .

  Definition range (length: nat): Seq (nat) :=
    _rec_range (length) ( nil ) .

End RecursionForCoq.



Module RecursionForCoqSpec.

  Import RecursionForCoq.

  Definition example_initial_value: nat := 0.

  Definition example_list_of_elements: list nat :=
    0 :: 1 :: 1 :: 2 :: 3 :: 5 :: 8 :: 13 :: 21 :: 34 :: 55 :: nil.

  Definition example_sum (x: nat) (y: nat): nat := x + y.

  Definition example_true (x: nat) (y: nat): bool := true.

  Definition example_evaluation_all: nat :=
    fold4 example_list_of_elements example_initial_value example_sum example_true.

  Definition example_limited (x: nat) (y: nat): bool := (x + y) <? 100.

  Definition example_evaluation_limited: nat :=
    fold4 example_list_of_elements example_initial_value example_sum example_limited.

  Eval compute in example_evaluation_all.
  (* = 143 : nat *)

  Eval compute in example_evaluation_limited.
  (* = 88 : nat *)

  Eval compute in (range 0).
  (* = nil : list nat *)

  Eval compute in (range 1).
  (* = 0 :: nil : list nat *)

  Eval compute in (range 10).
  (* = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: nil : list nat *)
 
End RecursionForCoqSpec.

End soda_example_forcoq.


