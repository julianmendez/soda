(* package soda.example *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)

Module soda_example_FactorialConcise.

Fixpoint rec_get_factorial (n: nat) (product: nat): nat :=
  match n with
    | 0 => product
    | S m => rec_get_factorial m (n * product)
  end.

Definition  get_factorial (n: nat): nat :=
  rec_get_factorial n 1.

End soda_example_FactorialConcise.

Import soda_example_FactorialConcise.

Inductive Pair :=
  | Pair_ (x: nat) (y: nat) .

Definition fst (p: Pair): nat :=
  match p with
    | Pair_ x y => x
  end.

Definition snd (p: Pair): nat :=
  match p with
    | Pair_ x y => y
  end.

Definition factorial_values: list Pair :=
    (Pair_ 0 1) :: (Pair_ 1 1) :: (Pair_ 2 2) :: (Pair_ 3 6) :: (Pair_ 4 24) :: (Pair_ 5 120) :: (Pair_ 6 720) :: nil.

Example test_1 :
  map get_factorial (map fst factorial_values) = map snd factorial_values.
Proof.
  compute.
  apply eq_refl.
Qed.



