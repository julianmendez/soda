(* soda.example.algorithms *)

Inductive PairExample : Type :=
    | PairExample_ (x: nat) (y: nat)
.

Module Type PairExample_Module.


  Parameter left : PairExample -> nat
.

  Parameter right : PairExample -> nat
.

End PairExample_Module.


Module Type SwapExample_Module <: PairExample_Module.


Definition    left (pair: PairExample ) : nat :=
    match pair with
      | (PairExample_ (x) (y ) ) => x
    end
.

Definition    right (pair: PairExample ) : nat :=
    match pair with
      | (PairExample_ (x) (y ) ) => y
    end
.

Definition    swap (pair: PairExample) : PairExample :=
    PairExample_ (right pair) (left pair )
.

Theorem    swap_of_swap: forall (pair: PairExample ), (swap (swap (pair ) ) ) = pair
.

Proof.
    intros p.
    destruct p.
    compute.
    apply eq_refl.
Qed.

End SwapExample_Module.



