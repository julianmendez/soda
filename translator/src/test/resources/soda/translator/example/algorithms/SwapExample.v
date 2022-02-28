package soda.example.algorithms

Module SimpleExample .

End SimpleExample .

Module PairExample .
  
Import SimpleExample .

  abstract
    left : Int
    right : Int

Inductive PairExample : Type :=
  | PairExample_ (x : Int * Int)
.

End PairExample .

Module SwapExample .

 Definition   left (pair : PairExample) : Int :=
    match pair with
      case (PairExample_ (x, y) ) => x
    end
.

 Definition   right (pair : PairExample) : Int :=
    match pair with
      case (PairExample_ (x, y) ) => y
    end
.

 Definition   swap (pair : PairExample) : PairExample :=
    PairExample_ (pair.right, pair.left)
.

Theorem    swap_of_swap : forall (pair : PairExample), (swap (swap (pair) ) ) == pair
.

Proof.
    intros p.
    destruct p.
    compute.
    apply eq_refl.
Qed.

End SwapExample .
