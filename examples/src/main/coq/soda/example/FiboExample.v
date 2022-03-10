(* package soda.example *)


Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Module soda_example.

Module FiboExample.

  Fixpoint rec_fib (m : nat) (a : nat) (b : nat) : nat :=
    match m with
      | 0 => a
      | 1 => b
      | S k => (rec_fib k b (a + b) )
    end.

  Definition fib (n : nat) :=
    rec_fib n 0 1.

End FiboExample.


Module FiboExampleSpec.

  Import FiboExample.

  Definition fibonacci_values : list (prod nat nat) :=
    (pair 0 0) :: (pair 1 1) :: (pair 2 1) :: (pair 3 2) :: (pair 4 3) :: (pair 5 5) :: (pair 6 8) :: (pair 7 13) :: (pair 8 21) :: (pair 9 34) :: (pair 10 55) :: (pair 11 89) :: (pair 12 144) :: nil.

  Example test_1 :
    map fib (map fst fibonacci_values) = map snd fibonacci_values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End FiboExampleSpec.

End soda_example.

