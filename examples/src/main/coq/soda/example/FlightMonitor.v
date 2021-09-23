(* package soda.example *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Ascii.
(* https://coq.inria.fr/library/Coq.Strings.Ascii.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


(** Date *)
Notation Date := nat.

Definition get_a_year_before (date: Date): Date :=
  date - 365.


(** Money *)
Notation Money := nat.


(** Airport *)
Inductive Airport : Type :=
  | Airport_ (a: ascii) (b: ascii) (c: ascii).


(** Segment *)
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


(** Flight *)
Inductive Flight : Type :=
  | SingleSegmentFlight_ (start_airport: Airport) (end_airport: Airport)
  | Flight_ (start_airport: Airport) (intermediate_stops: list Airport) (end_airport: Airport).

Definition flg_start_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => start_airport
    | Flight_ start_airport intermediate_stops end_airport => start_airport
  end.

Definition flg_end_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => end_airport
    | Flight_ start_airport intermediate_stops end_airport => end_airport
  end.

Fixpoint flg_segments_multi (start_airport: Airport) (intermediate_stops: list Airport) (end_airport: Airport): list Segment :=
  match intermediate_stops with
    | nil => (Segment_ start_airport end_airport) :: nil
    | x :: xs => (Segment_ start_airport x) :: (flg_segments_multi x xs end_airport)
  end.

Definition flg_segments (flight: Flight): list Segment :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => (Segment_ start_airport end_airport) :: nil
    | Flight_ start_airport intermediate_stops end_airport => (flg_segments_multi start_airport intermediate_stops end_airport)
  end.


(** MonitoringAgent *)

(** Rule 1 *)

Definition prices_of_segments (get_price : Flight -> Date -> Money) (segments: list Segment) (date: Date): list Money :=
  map (fun segment => (get_price (SingleSegmentFlight_ (seg_start_airport segment) (seg_end_airport segment) ) date) ) segments.

Definition sum_of_prices (prices: list Money): Money :=
  fold_left (fun x => (fun y => x + y)) prices 0.

Definition price_of_flight_by_segments (get_price : Flight -> Date -> Money) (flight: Flight) (date: Date): Money :=
  sum_of_prices (prices_of_segments get_price (flg_segments flight) date).

Definition complies_with_ethical_rule_1 (get_price : Flight -> Date -> Money) (flight: Flight) (date: Date): bool :=
  (get_price flight date) <=? (price_of_flight_by_segments get_price flight date).

(** Rule 2 *)

Definition is_price_increase_acceptable (old_price: nat) (new_price: nat): bool :=
    new_price <=? ((old_price * 125) / 100).

Definition complies_with_ethical_rule_2 (get_price: Flight -> Date -> Money) (flight: Flight) (date: Date): bool :=
  is_price_increase_acceptable (get_price flight date) (get_price flight (get_a_year_before date) ).


