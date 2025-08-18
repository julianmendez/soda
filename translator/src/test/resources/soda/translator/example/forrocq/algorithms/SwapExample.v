Module PairExample .

Class PairExample : Type :=

  mk {
    left : nat ;
    right : nat
} .

Notation "'PairExample_'" := PairExample.mk (at level 99) .

End PairExample .

Import PairExample .

Module SwapExample .

Class SwapExample : Type :=

  mk {
    
} .

Notation "'SwapExample_'" := SwapExample.mk (at level 99) .

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
