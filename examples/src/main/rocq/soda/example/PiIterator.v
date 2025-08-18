(* package soda.example *)


Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)

Require Import Rocq.ZArith.BinInt.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.ZArith.BinInt.html *)


Module soda_example_piiterator.

Notation BigInt := Z.

Module Status.

  Inductive type :=
    | Status_ (r : BigInt) (n : nat) (q : BigInt) (t : BigInt) (l : nat) (k : nat).

  Definition r (s : Status.type) : BigInt :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => r0
    end.

  Definition n (s : Status.type) : nat :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => n0
    end.

  Definition n_ (s : Status.type) : BigInt :=
    Z_of_nat (n s).

  Definition q (s : Status.type) : BigInt :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => q0
    end.

  Definition t (s : Status.type) : BigInt :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => t0
    end.

  Definition l (s : Status.type) : nat :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => l0
    end.

  Definition l_ (s : Status.type) : BigInt :=
    Z_of_nat (l s).

  Definition k (s : Status.type) : nat :=
    match s with
      | Status_ r0 n0 q0 t0 l0 k0 => k0
    end.

  Definition k_ (s : Status.type) : BigInt :=
    Z_of_nat (k s).

End Status.


Module BigIntAndStatus.

  Inductive type :=
    | BigIntAndStatus_ (d : nat) (new_status : Status.type).

  Definition digit (s : BigIntAndStatus.type) : nat :=
    match s with
      | BigIntAndStatus_ digit0 new_status0 => digit0
    end.

  Definition new_status (s : BigIntAndStatus.type) : Status.type :=
    match s with
      | BigIntAndStatus_ digit0 new_status0 => new_status0
    end.

End BigIntAndStatus.

Module PiIterator.

  Local Open Scope Z_scope.
  Import Z.

  Definition initial_status : Status.type :=
    (Status.Status_ 0 3 1 1 3 1).

  Notation "s .r" := (Status.r s) (at level 9).
  Notation "s .n" := (Status.n s) (at level 9).
  Notation "s .n_" := (Status.n_ s) (at level 9).
  Notation "s .q" := (Status.q s) (at level 9).
  Notation "s .t" := (Status.t s) (at level 9).
  Notation "s .l" := (Status.l s) (at level 9).
  Notation "s .l_" := (Status.l_ s) (at level 9).
  Notation "s .k" := (Status.k s) (at level 9).
  Notation "s .k_" := (Status.k_ s) (at level 9).

  Definition bound_of_recursions (s : Status.type) : nat :=
    s.l .

  Fixpoint _tailrec_compute_new_status (c : nat) (s : Status.type) : Status.type :=
    match c with
      | O => s
      | S new_counter =>
          if (4 * s.q + s.r - s.t) <? (s.n_ * s.t)
          then s
          else
            let r := (2 * s.q + s.r) * s.l_
            in let n := abs_nat ( (s.q * (7 * s.k_) + 2 + (s.r * s.l_)) / (s.t * s.l_) )
            in let q := s.q * s.k_
            in let t := s.t * s.l_
            in let l := abs_nat (s.l_ + 2)
            in let k := abs_nat (s.k_ + 1)
            in let new_status := (Status.Status_ r n q t l k)
            in (_tailrec_compute_new_status new_counter new_status)
    end.

  Definition compute_new_status (s : Status.type) : Status.type :=
    _tailrec_compute_new_status (bound_of_recursions s) s.

  Definition _get_next (s : Status.type) : BigIntAndStatus.type :=
    let ns := (compute_new_status s)
    in let ret := abs_nat ns.n_
    in let r := 10 * (ns.r - ns.n_ * ns.t )
    in let n := abs_nat ( ((10 * (3 * ns.q + ns.r)) / ns.t) - (10 * ns.n_) )
    in let q := ns.q  * 10
    in let t := ns.t
    in let l := ns.l
    in let k := ns.k
    in let new_status := (Status.Status_ r n q t l k)
    in (BigIntAndStatus.BigIntAndStatus_ ret new_status).

  Fixpoint _tailrec_take (n : nat) (rev_seq : list nat)
      (s : Status.type) : (prod (list nat) Status.type) :=
    match n with
      | O => (pair (rev rev_seq) s)
      | S m =>
          let bs := (_get_next s)
          in (_tailrec_take m ((BigIntAndStatus.digit bs) :: rev_seq) (BigIntAndStatus.new_status bs))
    end.

  Definition take (n : nat) : (list nat) :=
    fst (_tailrec_take n nil initial_status).

End PiIterator.

Module PiIteratorSpec.

  Import PiIterator.

  Definition piStart : list nat :=
    3 :: 1 :: 4 :: 1 :: 5 :: 9 :: 2 :: 6 :: 5 :: 3 :: 5 :: 8 :: nil.

  Definition piSequence : list nat := PiIterator.take 12.

  Example test1 : piSequence = piStart.
  Proof.
    compute.
    reflexivity.
  Qed.

End PiIteratorSpec.

End soda_example_piiterator.


