(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).


Module soda_example_forcoq.

Module FiboExampleInSodaForCoq.

  Fixpoint _tailrec_fib (m : nat) (a : nat) (b : nat) : nat :=
    match m with
      | O() => a
      | S (O()) => b
      | S (k) => _tailrec_fib (k) (b) (a .add (b))
    end.

  Definition fib (n : nat) :=
    _tailrec_fib (n) (O()) ( S (O() ) ) .

End FiboExampleInSodaForCoq.

End soda_example_forcoq.


