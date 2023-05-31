
module Fairness where

open import Data.Bool using (Bool ; false ; true ; if_then_else_)
open import Data.Float using (Float ; _-_ ; _<ᵇ_)


record Applicant : Set where
  constructor Applicant-
  field
    background-score : Float

open Applicant


record Fairness : Set where
  constructor Fairness-
  field
    score-difference-tolerance : Float
    ranking-difference-tolerance : Float
    rank : Applicant -> Float


  abs-value : (value : Float) -> Float
  abs-value (value) =
    if value <ᵇ 0.0
    then 0.0 - value
    else value

  difference-between : (value : Float) -> (another-value : Float) -> Float
  difference-between (value) (another-value) =
    abs-value (value - another-value)

  have-similar-ranking : (result1 : Float) -> (result2 : Float) -> Bool
  have-similar-ranking (result1) (result2) =
    difference-between (result1) (result2) <ᵇ ranking-difference-tolerance

  have-similar-score : (score1 : Float) -> (score2 : Float) -> Bool
  have-similar-score (score1) (score2) =
    difference-between (score1) (score2) <ᵇ score-difference-tolerance

  is-fair : (alice : Applicant) (bob : Applicant) -> Bool
  is-fair (alice) (bob) =
    if have-similar-score (alice .background-score) (bob .background-score)
    then have-similar-ranking (rank (alice) ) (rank (bob) )
    else true


