(* package soda.example *)


Module soda_example_SwapExample.

Inductive PairExample : Type :=
  | PairExample_ (x: nat) (y: nat) .

Definition left (pair: PairExample) :=
  match pair with
    | (PairExample_ x y) => x
  end.

Definition right (pair: PairExample) :=
  match pair with
    | (PairExample_ x y) => y
  end.

Definition swap (pair: PairExample): PairExample :=
  (PairExample_ (right pair) (left pair)).


Lemma swap_of_swap : forall (pair: PairExample), swap (swap pair) = pair.
Proof.
  intros p.
  destruct p.
  compute.
  apply eq_refl.
Qed.

End soda_example_SwapExample.


Module soda_example_SwapExampleSpec.

Import soda_example_SwapExample.

Example test_1 : swap (PairExample_ 2 3) = (PairExample_ 3 2).
Proof.
  compute.
  apply eq_refl.
Qed.


Example test_2 : swap (swap (PairExample_ 2 3)) = (PairExample_ 2 3).
Proof.
  compute.
  apply eq_refl.
Qed.

End soda_example_SwapExampleSpec.


