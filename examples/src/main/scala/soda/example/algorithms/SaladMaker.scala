package soda.example.algorithms

trait SaladMaker {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_prepare_vegan_salad [Ingredient, Salad, VeganSalad <: Salad] (ingredients_so_far: Seq [Ingredient], salad_so_far: VeganSalad, next_ingredient_function: (Salad, Ingredient ) => VeganSalad, condition_to_continue: (Salad, Ingredient ) => Boolean ): Salad =
    if (ingredients_so_far.isEmpty
    ) salad_so_far
    else
      if (! condition_to_continue (salad_so_far, ingredients_so_far.head )
      ) salad_so_far
      else _tailrec_prepare_vegan_salad (ingredients_so_far.tail, next_ingredient_function (salad_so_far, ingredients_so_far.head ), next_ingredient_function, condition_to_continue )

  def prepare_vegan_salad [Ingredient, Salad, VeganSalad <: Salad] (list_of_ingredients: Seq [Ingredient], initial_bowl: VeganSalad, next_ingredient_function: (Salad, Ingredient ) => VeganSalad, condition_to_continue: (Salad, Ingredient ) => Boolean ): Salad =
    _tailrec_prepare_vegan_salad (list_of_ingredients, initial_bowl, next_ingredient_function, condition_to_continue )

}

case class SaladMaker_ ()
  extends SaladMaker
