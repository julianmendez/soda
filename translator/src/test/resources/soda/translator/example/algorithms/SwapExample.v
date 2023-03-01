Module PairExample .

(*
  abstract
    left : Int
    right : Int
*)

Inductive PairExample : Type :=
  | PairExample_ (x : Int * Int)
.

End PairExample .

Import PairExample .

Module SwapExample .

 Definition   left (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x , y) ) => x
    end
.

 Definition   right (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x , y) ) => y
    end
.

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (right (pair) , left (pair) )
.

Theorem    swap_of_swap : forall (pair : PairExample) , (swap (swap (pair) ) ) = pair
.

Proof.
    intros p.
    destruct p.
    compute.
    destruct x.
    apply eq_refl.
Qed.

End SwapExample .

Import SwapExample .
