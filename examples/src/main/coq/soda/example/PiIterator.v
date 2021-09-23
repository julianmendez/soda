(* package soda.example *)


Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)

Require Import Coq.ZArith.BinInt.
(* https://coq.inria.fr/library/Coq.ZArith.BinInt.html *)


Module soda_example_PiIterator.

Notation BigInt := Z.

Inductive Status :=
  | Status_ (r: BigInt) (n: nat) (q: BigInt) (t: BigInt) (l: nat) (k: nat).

Definition sr (s: Status): BigInt :=
  match s with
    | Status_ r n q t l k => r
  end.

Definition sn (s: Status): nat :=
  match s with
    | Status_ r n q t l k => n
  end.

Definition sn_ (s: Status): BigInt :=
  Z_of_nat (sn s).

Definition sq (s: Status): BigInt :=
  match s with
    | Status_ r n q t l k => q
  end.

Definition st (s: Status): BigInt :=
  match s with
    | Status_ r n q t l k => t
  end.

Definition sl (s: Status): nat :=
  match s with
    | Status_ r n q t l k => l
  end.

Definition sl_ (s: Status): BigInt :=
  Z_of_nat (sl s).

Definition sk (s: Status): nat :=
  match s with
    | Status_ r n q t l k => k
  end.

Definition sk_ (s: Status): BigInt :=
  Z_of_nat (sk s).


Inductive BigIntAndStatus :=
  | BigIntAndStatus_ (d: nat) (new_status: Status).

Definition bs_digit (s: BigIntAndStatus): nat :=
  match s with
    | BigIntAndStatus_ d new_status => d
  end.

Definition bs_new_status (s: BigIntAndStatus): Status :=
  match s with
    | BigIntAndStatus_ d new_status => new_status
  end.


Local Open Scope Z_scope.
Import Z.

Definition initial_status: Status :=
  (Status_ 0 3 1 1 3 1).

Lemma nice : (1 * 1) = 1.
Proof.
  compute.
  reflexivity.
Qed.

Definition bound_of_recursions (s: Status): nat :=
  sl s.



Fixpoint rec_compute_new_status (c: nat) (s: Status): Status :=
  match c with
    | O => s
    | S new_counter =>
        if (4 * (sq s) + (sr s) - (st s)) <? ((sn_ s) * (st s))
        then s
        else
          let r := (2 * (sq s) + (sr s)) * (sl_ s)
          in let n := abs_nat ( ((sq s) * (7 * (sk_ s)) + 2 + ((sr s) * (sl_ s))) / ((st s) * (sl_ s)) )
          in let q := (sq s) * (sk_ s)
          in let t := (st s) * (sl_ s)
          in let l := abs_nat ((sl_ s) + 2)
          in let k := abs_nat ((sk_ s) + 1)
          in let new_status := (Status_ r n q t l k)
          in (rec_compute_new_status new_counter new_status)
  end.

Definition compute_new_status (s: Status): Status :=
  rec_compute_new_status (bound_of_recursions s) s.

Definition _get_next (s: Status): BigIntAndStatus :=
    let ns := (compute_new_status s)
    in let ret := abs_nat (sn_ ns)
    in let r := 10 * ((sr ns) - (sn_ ns) * (st ns))
    in let n := abs_nat ( ((10 * (3 * (sq ns) + (sr ns))) / (st ns)) - (10 * (sn_ ns)) )
    in let q := (sq ns) * 10
    in let t := (st ns)
    in let l := (sl ns)
    in let k := (sk ns)
    in let new_status := (Status_ r n q t l k)
    in (BigIntAndStatus_ ret new_status).

Fixpoint rec_take (n: nat) (rev_seq: list nat) (s: Status): (prod (list nat) Status) :=
  match n with
    | O => (pair (rev rev_seq) s)
    | S m =>
        let bs := (_get_next s)
        in (rec_take m ((bs_digit bs) :: rev_seq) (bs_new_status bs))
  end.

Definition take (n: nat): (list nat) :=
  fst (rec_take n nil initial_status).


End soda_example_PiIterator.


Module soda_example_PiIteratorSpec.

Import soda_example_PiIterator.

Definition piStart: list nat := 3 :: 1 :: 4 :: 1 :: 5 :: 9 :: 2 :: 6 :: 5 :: 3 :: 5 :: 8 :: nil.

Definition piSequence: list nat := take 12.

Example test1 : piSequence = piStart.
Proof.
  compute.
  reflexivity.
Qed.

End soda_example_PiIteratorSpec.


