(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)


Notation "nat_O()" := O.

Notation "a .plus b" := (a + b) (at level 85).

Notation "(nat_S a )" := (S ( a )) (at level 85).


Fixpoint rec_fib (m: nat) (a: nat) (b: nat): nat :=
  match m with
    | nat_O() => a
    | (nat_S (nat_O())) => b
    | (nat_S (k)) => rec_fib (k) (b) (a .plus (b))
  end.


Definition fib (n: nat) :=
  rec_fib (n) (nat_O()) ( (nat_S (O)) ).

