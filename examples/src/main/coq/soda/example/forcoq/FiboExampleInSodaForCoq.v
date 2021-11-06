(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).


Fixpoint rec_fib (m: nat) (a: nat) (b: nat): nat :=
    match m with
      | O() => a
      | S (O()) => b
      | S (k) => rec_fib (k) (b) (a .add (b))
    end.

Definition fib (n: nat) :=
    rec_fib (n) (O()) ( S (O() ) ) .

