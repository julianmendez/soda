(* package soda.example *)


Require Import Rocq.Init.Nat.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Init.Nat.html *)

Require Import Rocq.Strings.String.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Strings.String.html *)


Module soda_example.

Module PatternMatching.

  Inductive Param : Type :=
    | Singleton_ (x : nat)
    | Pair_ (x : nat) (y : nat)
    | Triplet_ (x : nat) (y : nat) (z : nat).

  Definition name (elem : Param) : string :=
    match elem with
      | Singleton_ x => "singleton"
      | Pair_ x y => "pair"
      | Triplet_ x y z => "triplet"
    end.

  Definition get_value (p : Param) : nat :=
    match p with
      | Singleton_ x => x
      | Pair_ x y => (x + y) / 2
      | Triplet_ x y z => (x + y + z) / 3
    end.

  Definition get_type_name (p : Param) : string :=
    match p with
      | Singleton_ x => ((name p) ++ "(x)"%string)
      | Pair_ x y => ((name p) ++ "(x, y)"%string)
      | Triplet_ x y z => ((name p) ++ "(x, y, z)"%string)
    end.

End PatternMatching.


Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)


Module PatternMatchingSpec.

  Import PatternMatching.

  Definition values : list (prod Param nat) :=
    (pair (Singleton_ 5) 5) :: (pair (Pair_ 10 100) 55) ::
    (pair (Triplet_ 9 100 890) 333) :: nil.

  Example test_1 :
    map get_value (map fst values) = map snd values.
  Proof.
    compute.
    apply eq_refl.
  Qed.

End PatternMatchingSpec.

End soda_example.

