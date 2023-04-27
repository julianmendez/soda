module SwapExample where


import Relation.Binary.PropositionalEquality as Eq

open Eq using (_≡_)
open Eq.≡-Reasoning using (begin_ ; _≡⟨⟩_ ; _∎)

open import Data.Nat using (ℕ)


record PairExample : Set where
  constructor PairExample-
  field
    left : ℕ
    right : ℕ

open PairExample


record SwapExample : Set where
  constructor SwapExample-

  swap : (pair : PairExample) -> PairExample
  swap (pair) =
    PairExample- (pair .right) (pair .left)

  lemmaSwapOfSwap : ∀ (pair : PairExample) -> swap (swap (pair) ) ≡ pair

  lemmaSwapOfSwap (PairExample- (x) (y) ) =
    begin
      swap (swap (PairExample- (x) (y) ) )
    ≡⟨⟩
      swap (PairExample- ( (PairExample- (x) (y) ) .right) ( (PairExample- (x) (y) ) .left) )
    ≡⟨⟩
      swap (PairExample- (y) (x))
    ≡⟨⟩
      PairExample- ( (PairExample- (y) (x) ) .right) ( (PairExample- (y) (x) ) .left)
    ≡⟨⟩
      PairExample- (x) (y)
    ∎

