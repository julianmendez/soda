(* package soda.example *)


Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Module soda_example.

Module FactorialWithFold.

  Definition get_factorial (n : nat) : nat :=
    fold_left (fun (product : nat) (k : nat) => (product * (k + 1)) ) (seq 0 n) 1 .

End FactorialWithFold.


Module FactorialWithFoldSpec.

  Import FactorialWithFold.

  Definition factorial_values : list (prod nat nat) :=
    (pair 0 1) :: (pair 1 1) :: (pair 2 2) :: (pair 3 6) :: (pair 4 24) :: (pair 5 120) :: (pair 6 720) :: nil.

  Example test_1 :
    map get_factorial (map fst factorial_values) = map snd factorial_values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End FactorialWithFoldSpec.

End soda_example.

