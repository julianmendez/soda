Module PairExample .

Class PairExample : Type :=
  PairExample_ {
    left : nat ;
    right : nat
}.

End PairExample .

Import PairExample .

Module SwapExample .

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (pair .(right) ) (pair .(left) )
.

  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x) (y) ) ) ) =
    PairExample_ (x) (y) .
  Proof.
    auto.
  Qed.

End SwapExample .

Import SwapExample .
