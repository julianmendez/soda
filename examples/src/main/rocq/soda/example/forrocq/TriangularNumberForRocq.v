(* package soda.example.forrocq *)


Require Import Rocq.Init.Nat.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Init.Nat.html *)

Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).

Notation "a .mul b" := (mul a b) (at level 90).


Module soda_example_forrocq.

Module TriangularNumberForRocq.

  Fixpoint _tailrec_get_number (m : nat) (acc : nat) : nat :=
    match m with
      | O() => acc
      | S (k) => _tailrec_get_number (k) (acc .add (S (k) ))
    end.

  Definition get_number (n : nat) : nat :=
    _tailrec_get_number (n) (O() ) .

End TriangularNumberForRocq.

Import TriangularNumberForRocq.


  Definition triangular_number_values : list (prod nat nat) :=
    (pair 0 0) :: (pair 1 1) :: (pair 2 3) :: (pair 3 6) :: (pair 4 10) ::
      (pair 5 15) :: (pair 6 21) :: (pair 7 28) :: (pair 8 36) :: (pair 9 45) ::
      (pair 10 55) :: (pair 11 66) :: (pair 12 78) :: nil.

  Example test_1 :
    map get_number (map fst triangular_number_values) = map snd triangular_number_values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End soda_example_forrocq.

