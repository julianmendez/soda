package soda.example


trait SaladIngredient extends soda.lib.EnumConstant

case class SaladIngredient_ (ordinal: Int, name: String ) extends SaladIngredient

trait SaladIngredientConstant {

  lazy val tomato = SaladIngredient_ (1, "tomato")
  lazy val lettuce = SaladIngredient_ (2, "lettuce")
  lazy val sunflower_seeds = SaladIngredient_ (3, "sunflower seeds")
  lazy val olive_oil = SaladIngredient_ (4, "olive_oil")

  lazy val SaladIngredient_values = Seq (tomato, lettuce, sunflower_seeds, olive_oil )
}

case class SaladMakerSpec ()  extends org.scalatest.funsuite.AnyFunSuite  with SaladIngredientConstant {

  def add_next_ingredient (salad_so_far: Seq [SaladIngredient], ingredient: SaladIngredient ): Seq [SaladIngredient] =
    salad_so_far.+: (ingredient )

  def has_salad_at_most_2_ingredients (salad_so_far: Seq [SaladIngredient], next_ingredient: SaladIngredient ): Boolean =
    salad_so_far.length < 3

  test ("salad maker") {
    lazy val instance = SaladMaker_ ()
    lazy val ingredients = SaladIngredient_values
    lazy val expected = Seq (sunflower_seeds, lettuce, tomato )
    lazy val obtained = instance.prepare_vegan_salad (list_of_ingredients = ingredients, initial_bowl = Seq [SaladIngredient]  (), next_ingredient_function = add_next_ingredient, condition_to_continue = has_salad_at_most_2_ingredients    )

    assert (obtained == expected )
  }
}
