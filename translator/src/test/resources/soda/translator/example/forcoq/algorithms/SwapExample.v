Module PairExample .

Class PairExample : Type :=
  PairExample_ {
    left : nat ;
    right : nat
}.

End PairExample .

Import PairExample .

Module SwapExample .

  Definition left (pair : PairExample) : nat := pair .(left) .

  Definition right (pair : PairExample) : nat := pair .(right) .

(*
  directive scala
  def left (pair : PairExample) : nat = pair .left
*)

(*
  directive scala
  def right (pair : PairExample) : nat = pair .right
*)

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (right (pair) ) (left (pair) )
.

  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x) (y) ) ) ) =
    PairExample_ (x) (y) .
  Proof.
    auto.
  Qed.

End SwapExample .

Import SwapExample .
