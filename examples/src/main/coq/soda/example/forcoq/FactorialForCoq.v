(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).

Notation "a .mul b" := (mul a b) (at level 90).


Module soda_example_forcoq.

Module FactorialForCoq.


Fixpoint rec (m: nat) (product: nat): nat :=
  match m with
    | O() => product
    | S (k) => rec (k) (product .mul ( S (k) ))
  end.

Definition get_factorial (n: nat): nat :=
  rec (n) (S ( O() ) ) .


End FactorialForCoq.

End soda_example_forcoq.



