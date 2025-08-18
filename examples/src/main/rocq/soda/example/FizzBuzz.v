(* package soda.example *)


Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)

Require Import Rocq.Strings.String.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Strings.String.html *)

Require Import Rocq.Strings.Ascii.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Strings.Ascii.html *)

Require Import Rocq.Init.Nat.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Init.Nat.html *)



Module soda_example_fizzbuzz.


(** StringOfNat is a definition of [string_of_nat] to convert natural numbers to strings. *)

Module StringOfNat.

  Definition ascii_of_digit (n : nat) : ascii :=
    ascii_of_nat (n + 48).

  Definition is_digit (n : nat) : bool :=
    andb (0 <=? n) (n <=? 9).

  Fixpoint rec_string_of_nat (counter : nat) (n : nat) (acc : string) : string :=
    match counter with
      | 0 => EmptyString
      | S c =>
        if (is_digit n)
        then String (ascii_of_digit n) acc
        else rec_string_of_nat c (n / 10) (String (ascii_of_digit (n mod 10)) acc)
    end.
  (** The counter is only used to ensure termination. *)

  Definition string_of_nat (n : nat) : string :=
    rec_string_of_nat n n EmptyString.

End StringOfNat.


(** The FizzBuzz problem. *)

Module FizzBuzz.

  Definition fizz : string :=
    "Fizz" .

  Definition buzz : string :=
    "Buzz" .

  Definition new_line : string :=
    String (ascii_of_nat 10) EmptyString.

  Definition is_divisible_by (n : nat) (k : nat) : bool :=
    (n mod k) =? 0.

  Definition get_fizz_buzz_term (n : nat) : string :=
    if (is_divisible_by n 15) then fizz ++ buzz
    else if (is_divisible_by n 3) then fizz
    else if (is_divisible_by n 5) then buzz
    else (StringOfNat.string_of_nat n).

  Definition get_first_positive_numbers (n : nat) : list nat :=
    seq 1 n.

  Definition fizz_buzz_terms (n : nat) : list string :=
    map get_fizz_buzz_term (get_first_positive_numbers n).

  Definition fizz_buzz : string :=
    concat new_line (fizz_buzz_terms 100).

End FizzBuzz.


Import FizzBuzz.

(** This shows the string. *)
Eval compute in fizz_buzz.

End soda_example_fizzbuzz.

