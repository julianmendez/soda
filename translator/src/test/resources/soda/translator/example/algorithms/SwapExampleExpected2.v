(* package soda.example.algorithms *)

(* class PairExample *)

(*  abstract
    left : Int
    right : Int *)

Notation Int := nat.

Inductive PairExample : Type :=
  | PairExample_ (x : Int * Int)
.

(* end *)

(* class SwapExample *)

 Definition   left (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x, y) ) => x
    end
.

 Definition   right (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x, y) ) => y
    end
.

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (right (pair), left (pair) )
.

Theorem    swap_of_swap : forall (pair : PairExample), (swap (swap (pair) ) ) = pair
.

Proof.
    intros p.
    destruct p.
    compute.
    apply eq_refl.
Qed.

(* end *)
