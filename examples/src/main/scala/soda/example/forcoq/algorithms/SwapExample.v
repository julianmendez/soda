Module PairExample .

Class PairExample : Type :=
  mk {
    left : nat ;
    right : nat
}.

Notation "'PairExample_'" := PairExample.mk (at level 99) .

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
