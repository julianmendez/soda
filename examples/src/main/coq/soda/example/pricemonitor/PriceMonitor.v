(* package soda.example.pricemonitor *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Ascii.
(* https://coq.inria.fr/library/Coq.Strings.Ascii.html *)

Require Import Coq.Strings.String.
(* https://coq.inria.fr/library/Coq.Strings.String.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)

Require Import Coq.Floats.Floats.
(* https://coq.inria.fr/library/Coq.Floats.Floats.html *)



Module soda_example_pricemonitor_PriceMonitor.


(** Date *)
Notation Date := nat.


(** Money *)
Notation Money := nat.


(** Airport *)
Inductive Airport: Type :=
  | Airport_ (a: ascii) (b: ascii) (c: ascii).


(** Customer *)
Inductive Customer: Type :=
  | Customer_ (name: string) (ip_address: string).



(** Flight *)
Inductive Flight : Type :=
  | SingleSegmentFlight_ (start_airport: Airport) (end_airport: Airport)
  | Flight_ (start_airport: Airport) (intermediate_airports: list Airport) (end_airport: Airport).

Definition flg_start_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => start_airport
    | Flight_ start_airport intermediate_airports end_airport => start_airport
  end.

Definition flg_intermediate_airports (flight: Flight): list Airport :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => nil
    | Flight_ start_airport intermediate_airports end_airport => nil
  end.

Definition flg_end_airport (flight: Flight): Airport :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => end_airport
    | Flight_ start_airport intermediate_airports end_airport => end_airport
  end.


(** PricingAgent *)
Definition get_price (customer: Customer) (flight: Flight) (date_in_days: Date): Money :=
  100 * (length (flg_intermediate_airports flight)).



Definition milliseconds_per_day: nat :=
  24 * 60 * 60 * 1000.

Fixpoint float_of_nat (n: nat): float :=
  match n with
    | O => 0
    | S m => 1 + (float_of_nat m)
  end.

End soda_example_pricemonitor_PriceMonitor.



Module soda_example_pricemonitor_Requirement1Monitor.

Import soda_example_pricemonitor_PriceMonitor.


Inductive Report1: Type :=
  | Report1_ (compliant: bool) (price_for_c1: Money) (price_for_c2: Money) (similarity: float).

Definition min (x: nat) (y: nat): nat :=
  if x <? y then x else y.

Definition max (x: nat) (y: nat): nat :=
  if x <? y then y else x.

Definition minimum_acceptable_similarity: float := 0.95 .


Definition get_report (c1: Customer) (c2: Customer) (flight: Flight) (date_in_days: Date): Report1 :=
  let
      price_for_c1 := (get_price c1 flight date_in_days)
  in let
      price_for_c2 := (get_price c2 flight date_in_days)
  in let
      similarity :=  (div (float_of_nat (min price_for_c1 price_for_c2) ) (float_of_nat (max price_for_c1 price_for_c2) ) )
  in Report1_ (leb minimum_acceptable_similarity similarity) price_for_c1 price_for_c2 similarity.


End soda_example_pricemonitor_Requirement1Monitor.


Module soda_example_pricemonitor_Requirement2Monitor.

Import soda_example_pricemonitor_PriceMonitor.


Inductive Report2: Type :=
  | Report2_ (compliant: bool) (old_price: Money) (new_price: Money).


Definition get_a_year_before (date_in_days: Date): Date :=
  date_in_days - 365.

Definition acceptable_yearly_increase_percent: nat := 125 .

Definition get_report (customer: Customer) (flight: Flight) (date_in_days: Date): Report2 :=
  let
    old_price := (get_price customer flight (get_a_year_before date_in_days) )
  in let
    new_price := (get_price customer flight date_in_days)
  in Report2_ (new_price <=? ((old_price * acceptable_yearly_increase_percent) / 100)) old_price new_price.

End soda_example_pricemonitor_Requirement2Monitor.


Module soda_example_pricemonitor_Requirement3Monitor.

Import soda_example_pricemonitor_PriceMonitor.


(** Segment *)
Inductive Segment: Type :=
  | Segment_ (start_airport: Airport) (end_airport: Airport).

Definition seg_start_airport (segment: Segment): Airport :=
  match segment with
    | Segment_ start_airport end_airport => start_airport
  end.

Definition seg_end_airport (segment: Segment): Airport :=
  match segment with
    | Segment_ start_airport end_airport => end_airport
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


Inductive Report3: Type :=
  | Report3_ (compliant: bool) (price_of_flight: Money) (price_of_flight_by_segments: Money).


Definition sum_prices (prices: list Money): Money :=
  fold_left (fun x => (fun y => x + y)) prices 0.

Definition get_prices_of_segments (customer: Customer) (segments: list Segment) (date_in_days: Date): list Money :=
  map (fun segment => (get_price customer (SingleSegmentFlight_ (seg_start_airport segment) (seg_end_airport segment) ) date_in_days) ) segments.

Definition get_price_of_flight_by_segments (customer: Customer) (flight: Flight) (date_in_days: Date): Money :=
  sum_prices (get_prices_of_segments customer (flg_segments flight) date_in_days).

Definition get_report (customer: Customer) (flight: Flight) (date_in_days: Date): Report3 :=
  let
    price_of_flight := (get_price customer flight date_in_days)
  in let
    price_of_flight_by_segments := (get_price_of_flight_by_segments customer flight date_in_days)
  in Report3_ (price_of_flight <=? price_of_flight_by_segments) price_of_flight price_of_flight_by_segments.


End soda_example_pricemonitor_Requirement3Monitor.

