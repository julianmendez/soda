(* package soda.example *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Coq.Strings.String.
(* https://coq.inria.fr/library/Coq.Strings.String.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Inductive Airport : Type :=
  | Airport_ (name: string) .

Inductive Segment : Type :=
  | Segment_ (start_airport: Airport) (end_airport: Airport).

Definition seg_start_airport (segment: Segment): Airport :=
  match segment with
    | Segment_ start_airport end_airport => start_airport
  end.

Definition seg_end_airport (segment: Segment): Airport :=
  match segment with
    | Segment_ start_airport end_airport => end_airport
  end.

Inductive Flight : Type :=
  | SingleSegmentFlight_ (segment: Segment)
  | Flight_ (segment: Segment) (more_segments: list Segment).

Definition flg_start_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ segment => (seg_start_airport segment)
    | Flight_ segment more_segments => (seg_start_airport segment)
  end.

Fixpoint last_segment (head: Segment) (tail: list Segment): Segment :=
  match tail with
    | nil => head
    | (cons new_head new_tail) => last_segment new_head new_tail
  end.

Definition flg_end_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ segment => (seg_end_airport segment)
    | Flight_ segment more_segments => (seg_end_airport (last_segment segment more_segments))
  end.

Definition is_price_increase_acceptable (old_price: nat) (new_price: nat): bool :=
    (new_price <=? ((old_price * 125) / 100) ).

Notation Date := nat.

Definition get_a_year_before (date: Date): Date :=
  date - 365.

Notation Money := nat.

Definition complies_with_ethical_rule_2 (get_price: Flight -> Date -> Money) (flight: Flight) (date: Date): bool :=
  (is_price_increase_acceptable (get_price flight date) (get_price flight (get_a_year_before date) ) ).

Definition flg_segments (flight: Flight): list Segment :=
  match flight with
    | SingleSegmentFlight_ segment => segment :: nil
    | Flight_ segment more_segments => segment :: more_segments
  end.

Definition prices_of_segments (get_price : Flight -> Date -> Money) (segments: list Segment) (date: Date): list Money :=
  map (fun segment => (get_price (SingleSegmentFlight_ segment) date) ) segments.

Definition sum_of_prices (prices: list Money): Money :=
  fold_left (fun x => (fun y => x + y)) prices 0.

Definition price_of_flight_by_segments (get_price : Flight -> Date -> Money) (flight: Flight) (date: Date): Money :=
  sum_of_prices (prices_of_segments get_price (flg_segments flight) date).

Definition complies_with_ethical_rule_1 (get_price : Flight -> Date -> Money) (flight: Flight) (date: Date): bool :=
  (get_price flight date) <=? (price_of_flight_by_segments get_price flight date).

