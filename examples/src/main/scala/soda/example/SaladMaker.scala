package soda.example


trait SaladMaker {

  def prepare_vegan_salad [Ingredient, Salad, VeganSalad <: Salad]  (list_of_ingredients: Seq [Ingredient], initial_bowl: VeganSalad, next_ingredient_function: (Salad, Ingredient ) => VeganSalad, condition_to_continue: (Salad, Ingredient ) => Boolean  ): Salad =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (ingredients_so_far: Seq [Ingredient], salad_so_far: Salad ): Salad =
        if (ingredients_so_far.isEmpty
        ) salad_so_far
        else
          if (! condition_to_continue (salad_so_far, ingredients_so_far.head )
          ) salad_so_far
          else rec (ingredients_so_far.tail, next_ingredient_function (salad_so_far, ingredients_so_far.head )  )

      rec (list_of_ingredients, initial_bowl ) }

}

case class SaladMaker_ () extends SaladMaker
