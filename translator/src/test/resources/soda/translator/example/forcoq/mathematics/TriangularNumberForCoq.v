(*
package soda.example.forcoq.mathematics
*)

Require Import Coq.ZArith.BinInt .
(* https://coq.inria.fr/library/Coq.ZArith.BinInt.html *)

Require Import Coq.Lists.List .
(* https://coq.inria.fr/library/Coq.Lists.List.html *)

Notation Int := Z .


Module TriangularNumberForCoq .

  Import   soda.example.forcoq.lib.O_ .
  Import   soda.example.forcoq.lib.S_ .
  Import   soda.example.forcoq.lib.nat .

Fixpoint   @tailrec
   _tailrec_get_number (m : nat) (acc : nat) : nat :=
    match m with
      | S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      | x => acc
    end
.

 Definition   get_number (n : nat) : nat :=
    _tailrec_get_number (n) ( O_ () )
.

End TriangularNumberForCoq .

Import TriangularNumberForCoq .
