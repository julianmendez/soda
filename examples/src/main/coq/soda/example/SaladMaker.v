(* package soda.example *)


Require Import Coq.Init.Nat.
(* https://coq.inria.fr/library/Coq.Init.Nat.html *)

Require Import Coq.Strings.String.
(* https://coq.inria.fr/library/Coq.Strings.String.html *)

Require Import Coq.Lists.List.
(* https://coq.inria.fr/library/Coq.Lists.List.html *)


Module soda_example.

Module SaladMaker.

  Fixpoint prepare_salad
      (Ingredient : Type) (Salad : Type)
      (next_ingredient_function : Salad -> Ingredient -> Salad)
      (condition_to_continue : Salad -> Ingredient -> bool)
      (ingredients_so_far : list Ingredient)
      (salad_so_far : Salad) : Salad :=
    match ingredients_so_far with
      | nil => salad_so_far
      | head :: tail =>
          if ( negb (condition_to_continue salad_so_far head) )
        then salad_so_far
        else ( prepare_salad Ingredient Salad next_ingredient_function condition_to_continue tail (next_ingredient_function salad_so_far head) )
  end.

End SaladMaker.

Module SaladMakerSpec.

Import SaladMaker.

  Inductive SaladIngredient : Type :=
    | SaladIngredient_ (ordinal : nat) (name : string).


  Definition tomato := (SaladIngredient_ 1 "tomato").

  Definition lettuce := (SaladIngredient_ 2 "lettuce").

  Definition sunflower_seeds := (SaladIngredient_ 3 "sunflower seeds").

  Definition olive_oil := (SaladIngredient_ 4 "olive_oil").

  Definition SaladIngredient_values := tomato :: lettuce :: sunflower_seeds :: olive_oil :: nil.

  Definition add_next_ingredient (salad_so_far : list SaladIngredient) (ingredient : SaladIngredient) : list SaladIngredient :=
    ingredient :: salad_so_far.

  Definition has_salad_at_most_2_ingredients (salad_so_far : list SaladIngredient) (next_ingredient : SaladIngredient) : bool :=
    (length salad_so_far) <? 3.

  Definition ingredients := SaladIngredient_values.

  Definition expected := sunflower_seeds :: lettuce :: tomato :: nil.

  Definition obtained :=
      (prepare_salad SaladIngredient (list SaladIngredient)
        add_next_ingredient
        has_salad_at_most_2_ingredients
        ingredients
        nil).

  Example test1  : obtained = expected.
  Proof.
    reflexivity.
  Qed.

End SaladMakerSpec.

End soda_example.

