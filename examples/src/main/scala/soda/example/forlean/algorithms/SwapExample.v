Definition Nat : Type := nat .

Module PairExample .

Class PairExample : Type :=
  PairExample_ {
    left : Nat ;
    right : Nat
}.

End PairExample .

Import PairExample .

Module SwapExample .

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (pair .(right)) (pair .(left))
.

(*
  directive lean
  theorem
    swap_of_swap (x : Nat) (y : Nat) : (swap (swap (PairExample_ (x) (y) ) ) ) = PairExample_ (x) (y) :=
      by
        constructor
*)

  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x) (y) ) ) ) =
    PairExample_ (x) (y) .
  Proof.
    auto.
  Qed.

End SwapExample .

Import SwapExample .
