(* package soda.example.forcoq *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Notation "O()" := O.

Notation "a .add b" := (add a b) (at level 85).

Notation "a .mul b" := (mul a b) (at level 90).


Module soda_example_forcoq.

Module TriangularNumberForCoq.

  Fixpoint _rec_get_number (m: nat) (acc: nat): nat :=
    match m with
      | O() => acc
      | S (k) => _rec_get_number (k) (acc .add ( S (k) ))
    end.

  Definition get_number (n: nat): nat :=
    _rec_get_number (n) ( O() ) .

End TriangularNumberForCoq.

Import TriangularNumberForCoq.


  Definition triangular_number_values: list (prod nat nat) :=
    (pair 0 0) :: (pair 1 1) :: (pair 2 3) :: (pair 3 6) :: (pair 4 10) :: (pair 5 15) :: (pair 6 21) :: (pair 7 28) :: (pair 8 36) :: (pair 9 45) :: (pair 10 55) :: (pair 11 66) :: (pair 12 78) :: nil.

  Example test_1 :
    map get_number (map fst triangular_number_values) = map snd triangular_number_values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End soda_example_forcoq.

