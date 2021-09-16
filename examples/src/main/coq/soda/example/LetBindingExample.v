Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)

Require Import Coq.Strings.String.
(* https://coq.inria.fr/library/Coq.Strings.String.html *)


Definition three_parts_like_let_in :=
    let 
      part: string := " part"%string
    in let 
      first_part: string := (append "first"%string part)
    in let 
      second_part: string := (append "second"%string part)
    in let 
      third_part: string := (append "third"%string part)
    in (first_part :: (second_part :: (third_part :: nil))).


