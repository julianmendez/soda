(* package soda.example *)


Require Import Rocq.Lists.List.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Lists.List.html *)

Require Import Rocq.Strings.String.
(* https://rocq-prover.org/doc/v9.0/stdlib/Stdlib.Strings.String.html *)


Module soda_example.

Module LetBindingExample.

  Definition three_parts_like_let_in :=
    let 
      part : string := " part"%string
    in let 
      first_part : string := (append "first"%string part)
    in let 
      second_part : string := (append "second"%string part)
    in let 
      third_part : string := (append "third"%string part)
    in (first_part :: (second_part :: (third_part :: nil))).

End LetBindingExample.

End soda_example.

