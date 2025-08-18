(* package soda.example.forrocq *)


Require Import Rocq.Init.Nat.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Init.Nat.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).

Notation "a .mul b" := (mul a b) (at level 90).


Module soda_example_forrocq.

Module FactorialForRocq.

  Fixpoint _tailrec_get_factorial (m : nat) (product : nat) : nat :=
    match m with
      | O() => product
      | S (k) => _tailrec_get_factorial (k) (product .mul ( S (k) ))
    end.

  Definition get_factorial (n : nat) : nat :=
    _tailrec_get_factorial (n) (S ( O() ) ) .

End FactorialForRocq.

End soda_example_forrocq.



