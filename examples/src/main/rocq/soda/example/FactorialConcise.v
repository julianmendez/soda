(* package soda.example *)


Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)


Module soda_example.

  Module FactorialConcise.

  Fixpoint _tailrec_get_factorial (n : nat) (product : nat) : nat :=
    match n with
      | 0 => product
      | S m => _tailrec_get_factorial m (n * product)
    end.

  Definition get_factorial (n : nat) : nat :=
    _tailrec_get_factorial n 1.

End FactorialConcise.


Module FactorialConciseSpec.

  Import FactorialConcise.

  Definition factorial_values : list (prod nat nat) :=
    (pair 0 1) :: (pair 1 1) :: (pair 2 2) :: (pair 3 6) :: (pair 4 24) :: (pair 5 120) ::
    (pair 6 720) :: nil.

  Example test_1 :
    map get_factorial (map fst factorial_values) = map snd factorial_values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End FactorialConciseSpec.

End soda_example.


