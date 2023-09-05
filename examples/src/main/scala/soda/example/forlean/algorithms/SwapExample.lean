/-
directive coq
Definition Nat : Type := nat.
-/

namespace PairExample

class PairExample where
  PairExample_ ::
    left : Nat
    right : Nat
  deriving DecidableEq

namespace PairExample


end PairExample

open PairExample

namespace SwapExample

 def   swap (pair : PairExample) : PairExample :=
    PairExample_ (pair.right) (pair.left)


  theorem
    swap_of_swap (x : Nat) (y : Nat) : (swap (swap (PairExample_ (x) (y) ) ) ) = PairExample_ (x) (y) :=
      by
        constructor

/-
  directive coq
  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x) (y) ) ) ) =
    PairExample_ (x) (y).
  Proof.
    auto.
  Qed.
-/

end SwapExample

open SwapExample
