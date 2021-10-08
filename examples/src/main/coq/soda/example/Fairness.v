(* package soda.example *)


Require Import Coq.Floats.Floats.
(* https://coq.inria.fr/library/Coq.Floats.Floats.html *)

Require Import Coq.Floats.PrimFloat.
(* https://coq.inria.fr/library/Coq.Floats.PrimFloat.html *)



Module soda_example_fairness.

(** Applicant *)

Module Applicant.

Inductive type : Type :=
  | Applicant_ (score: float).

Definition score (applicant: Applicant.type): float :=
  match applicant with
    | Applicant_ s => s
  end.

End Applicant.


(** Fairness *)

Module Fairness.

Definition score_difference_tolerance: float := 0.125 .

Definition ranking_difference_tolerance: float := 0.5 .

Definition rank (applicant: Applicant.type): float :=
  (Applicant.score applicant) * 2.

Definition maximum_execution_time: float := 10 .

Definition measure_time (value: float): float := 1 .

Definition difference_between (value: float) (another_value: float): float :=
    (abs (value - another_value) ).

Definition have_similar_score (score1: float) (score2: float): bool :=
    (leb (difference_between score1 score2) score_difference_tolerance ).

Definition have_similar_ranking (result1: float) (result2: float) :=
    (leb (difference_between result1 result2) ranking_difference_tolerance ).


Definition is_response_time_acceptable (applicant: Applicant.type): bool :=
    if (leb (measure_time (rank applicant) ) maximum_execution_time )
    then true
    else false.

Definition is_fair (alice: Applicant.type) (bob: Applicant.type) :=
    if (have_similar_score (Applicant.score alice) (Applicant.score bob))
    then (have_similar_ranking (rank alice) (rank bob))
    else true.

End Fairness.

End soda_example_fairness.


