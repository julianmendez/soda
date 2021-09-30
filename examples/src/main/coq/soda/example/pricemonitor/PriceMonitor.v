(* package soda.example.pricemonitor *)


Require Import Ascii.
(* https://coq.inria.fr/library/Coq.Strings.Ascii.html *)

Require Import Coq.Strings.String.
(* https://coq.inria.fr/library/Coq.Strings.String.html *)

Require Import Coq.Floats.Floats.
(* https://coq.inria.fr/library/Coq.Floats.Floats.html *)

Require Import Coq.NArith.BinNat.
(* https://coq.inria.fr/library/Coq.NArith.BinNat.html *)


(** package soda.example.pricemonitor *)

Module soda_example_pricemonitor.


(** Date *)
Notation Date := N.


(** Money *)
Notation Money := N.


(** Customer *)

Module Customer.

Inductive type: Type :=
  | Customer_ (name: string) (ip_address: string).

Definition name (customer: Customer.type): string :=
  match customer with
    | Customer_ name ip_address => name
  end.

Definition ip_adress (customer: Customer.type): string :=
  match customer with
    | Customer_ name ip_address => ip_address
  end.

End Customer.


(** Airport *)
Module Airport.

Inductive type: Type :=
  | Airport_ (a: ascii) (b: ascii) (c: ascii).

End Airport.


(** Flight *)

Module Flight.

Inductive type : Type :=
  | SingleSegmentFlight_ (start_airport: Airport.type) (end_airport: Airport.type)
  | Flight_ (start_airport: Airport.type) (intermediate_airports: list Airport.type) (end_airport: Airport.type).

Definition start_airport (flight: Flight.type): Airport.type :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => start_airport
    | Flight_ start_airport intermediate_airports end_airport => start_airport
  end.

Definition intermediate_airports (flight: Flight.type): list Airport.type :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => nil
    | Flight_ start_airport intermediate_airports end_airport => intermediate_airports
  end.

Definition end_airport (flight: Flight.type): Airport.type :=
  match flight with
    | SingleSegmentFlight_ start_airport end_airport => end_airport
    | Flight_ start_airport intermediate_airports end_airport => end_airport
  end.

End Flight.


(** Report 1 *)

Module Report1. 

Inductive type: Type :=
  | Report1_ (compliant: bool) (price_for_c1: Money) (price_for_c2: Money) (similarity: float).

End Report1.


(** Requirement1Monitor *)

Module Requirement1Monitor.

Definition min (x: Money) (y: Money): Money :=
  if (N.ltb x y) then x else y.

Definition max (x: Money) (y: Money): Money :=
  if (N.ltb x y) then y else x.

Fixpoint float_of_nat (n: nat): float :=
  match n with
    | O => 0
    | S m => 1 + (float_of_nat m)
  end.

Definition float_of_N (n: N): float :=
  float_of_nat (N.to_nat n).

Definition minimum_acceptable_similarity: float := 0.95 .

Section has_PricingAgent.

Variable get_price: Customer.type -> Flight.type -> Date -> Money.

Definition get_report (c1: Customer.type) (c2: Customer.type) (flight: Flight.type) (date_in_days: Date): Report1.type :=
  let
      price_for_c1 := (get_price c1 flight date_in_days)
  in let
      price_for_c2 := (get_price c2 flight date_in_days)
  in let
      similarity :=  (div (float_of_N (min price_for_c1 price_for_c2) ) (float_of_N (max price_for_c1 price_for_c2) ) )
  in Report1.Report1_ (leb minimum_acceptable_similarity similarity) price_for_c1 price_for_c2 similarity.

End has_PricingAgent.

End Requirement1Monitor.


(** Report2 *)

Module Report2.

Inductive type: Type :=
  | Report2_ (compliant: bool) (old_price: Money) (new_price: Money).

End Report2.


(** Requirement2Monitor *)

Module Requirement2Monitor.

Definition get_a_year_before (date_in_days: Date): Date :=
  date_in_days - 365.

Definition acceptable_yearly_increase_percent: N := 125 .

Section has_PricingAgent.

Variable get_price: Customer.type -> Flight.type -> Date -> Money.

Definition get_report (customer: Customer.type) (flight: Flight.type) (date_in_days: Date): Report2.type :=
  let
    old_price := (get_price customer flight (get_a_year_before date_in_days) )
  in let
    new_price := (get_price customer flight date_in_days)
  in Report2.Report2_ (N.leb new_price ((old_price * acceptable_yearly_increase_percent) / 100)) old_price new_price.

End has_PricingAgent.

End Requirement2Monitor.


(** Segment *)

Module Segment.

(** Segment *)
Inductive type: Type :=
  | Segment_ (start_airport: Airport.type) (end_airport: Airport.type).

Definition start_airport (segment: Segment.type): Airport.type :=
  match segment with
    | Segment_ start_airport end_airport => start_airport
  end.

Definition end_airport (segment: Segment.type): Airport.type :=
  match segment with
    | Segment_ start_airport end_airport => end_airport
  end.

End Segment.

Module Report3.

Inductive type: Type :=
  | Report3_ (compliant: bool) (price_of_flight: Money) (price_of_flight_by_segments: Money).

End Report3.


(** Requirement3Monitor *)

Module Requirement3Monitor.

Module SegmentsForFlight.

Fixpoint rec_segments_multi (start_airport: Airport.type) (intermediate_stops: list Airport.type) (end_airport: Airport.type): list Segment.type :=
  match intermediate_stops with
    | nil => (Segment.Segment_ start_airport end_airport) :: nil
    | cons x xs => (Segment.Segment_ start_airport x) :: (rec_segments_multi x xs end_airport)
  end.

Definition segments (flight: Flight.type): list Segment.type :=
  match flight with
    | Flight.SingleSegmentFlight_ start_airport end_airport => (Segment.Segment_ start_airport end_airport) :: nil
    | Flight.Flight_ start_airport intermediate_stops end_airport => (rec_segments_multi start_airport intermediate_stops end_airport)
  end.

End SegmentsForFlight.

Definition sum_prices (prices: list Money): Money :=
  List.fold_left (fun x: Money => (fun y: Money => (N.add x y))) prices 0%N.

Section has_PricingAgent.

Variable get_price: Customer.type -> Flight.type -> Date -> Money.

Definition get_prices_of_segments (customer: Customer.type) (segments: list Segment.type) (date_in_days: Date): list Money :=
  List.map (fun segment => (get_price customer (Flight.SingleSegmentFlight_ (Segment.start_airport segment) (Segment.end_airport segment) ) date_in_days) ) segments.

Definition get_price_of_flight_by_segments (customer: Customer.type) (flight: Flight.type) (date_in_days: Date): Money :=
  sum_prices (get_prices_of_segments customer (SegmentsForFlight.segments flight) date_in_days).

Definition get_report (customer: Customer.type) (flight: Flight.type) (date_in_days: Date): Report3.type :=
  let
    price_of_flight := (get_price customer flight date_in_days)
  in let
    price_of_flight_by_segments := (get_price_of_flight_by_segments customer flight date_in_days)
  in Report3.Report3_ (N.leb price_of_flight price_of_flight_by_segments) price_of_flight price_of_flight_by_segments.

End has_PricingAgent.

End Requirement3Monitor.

End soda_example_pricemonitor.


(** tests for package soda.example.pricemonitor *)

Module soda_example_pricemonitor_test.

Import soda_example_pricemonitor.


Module PriceMonitorSpec.

Definition customer_1: Customer.type := Customer.Customer_ "Jon" "127.0.0.1".

Definition customer_2: Customer.type := Customer.Customer_ "Maria" "192.168.1.1".

Definition flight_1: Flight.type := Flight.Flight_ (Airport.Airport_  "B" "E" "R") ((Airport.Airport_  "F" "R" "A") :: (Airport.Airport_  "A" "R" "N") :: nil) (Airport.Airport_  "U" "M" "U").

Definition date_1: Date := 18898.

Definition unfair_get_price (customer: Customer.type) (flight: Flight.type) (date_as_days: Date): Money :=
    N.of_nat(String.length (Customer.name customer)) * ( (N.modulo date_as_days 100) + 100 * (N.of_nat (List.length (Flight.intermediate_airports flight))) + 1).

Definition fair_get_price (customer: Customer.type) (flight: Flight.type) (date_as_days: Date): Money :=
    100 * ( (N.of_nat (List.length (Flight.intermediate_airports flight))) + 1).

End PriceMonitorSpec.


Module UnfairPricingAgentSpec.

Import PriceMonitorSpec.

(** unfair pricing agent - requirement_monitor 1 *)
Definition test1_obtained := (Requirement1Monitor.get_report unfair_get_price customer_1 customer_2 flight_1 date_1).
Definition test1_expected := (Report1.Report1_ false 897 1495 0.6).

Example test1 : test1_obtained = test1_expected.
 Proof.
  compute.
  apply eq_refl.
Qed.

(** unfair pricing agent - requirement_monitor 2 *)
Definition test2_obtained := (Requirement2Monitor.get_report unfair_get_price customer_1 flight_1 date_1).
Definition test2_expected := (Report2.Report2_ false 702 897).

Example test2 : test2_obtained = test2_expected.
Proof.
  compute.
  apply eq_refl.
Qed.

(** unfair pricing agent - requirement_monitor 3 *)
Definition test3_obtained := (Requirement3Monitor.get_report unfair_get_price customer_1 flight_1 date_1).
Definition test3_expected := (Report3.Report3_ false 897 891).

Example test3 : test3_obtained = test3_expected.
Proof.
  compute.
  apply eq_refl.
Qed.

End UnfairPricingAgentSpec.


Module FairPricingAgentSpec.

Import PriceMonitorSpec.

(** fair pricing agent - requirement_monitor 1 *)
Definition test1_obtained := (Requirement1Monitor.get_report fair_get_price customer_1 customer_2 flight_1 date_1).
Definition test1_expected := (Report1.Report1_ true 300 300 1.0).

Example test1 : test1_obtained = test1_expected.
Proof.
  compute.
  apply eq_refl.
Qed.

(** fair pricing agent - requirement_monitor 2 *)
Definition test2_obtained := (Requirement2Monitor.get_report fair_get_price customer_1 flight_1 date_1).
Definition test2_expected := (Report2.Report2_ true 300 300).

Example test2 : test2_obtained = test2_expected.
Proof.
  compute.
  apply eq_refl.
Qed.

(** fair pricing agent - requirement_monitor 3 *)
Definition test3_obtained := (Requirement3Monitor.get_report fair_get_price customer_1 flight_1 date_1).
Definition test3_expected := (Report3.Report3_ true 300 300).

Example test3 : test3_obtained = test3_expected.
Proof.
  compute.
  apply eq_refl.
Qed.

End FairPricingAgentSpec.

End soda_example_pricemonitor_test.


