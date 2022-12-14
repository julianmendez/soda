package soda.example.algorithms

trait SaladIngredient
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class SaladIngredient_ (ordinal : Int, name : String) extends SaladIngredient

trait SaladIngredientConstant
{

  lazy val tomato = SaladIngredient_ (1, "tomato")

  lazy val lettuce = SaladIngredient_ (2, "lettuce")

  lazy val sunflower_seeds = SaladIngredient_ (3, "sunflower seeds")

  lazy val olive_oil = SaladIngredient_ (4, "olive_oil")

  lazy val SaladIngredient_values = Seq (tomato, lettuce, sunflower_seeds, olive_oil)

}

case class SaladIngredientConstant_ () extends SaladIngredientConstant

case class SaladMakerSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
    with SaladIngredientConstant
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def add_next_ingredient (salad_so_far : Seq [SaladIngredient] ) (ingredient : SaladIngredient) : Seq [SaladIngredient] =
    salad_so_far.+: (ingredient)

  def has_salad_at_most_2_ingredients (salad_so_far : Seq [SaladIngredient] ) (next_ingredient : SaladIngredient) : Boolean =
    salad_so_far.length < 3

  test ("salad maker") (
    check (
      obtained = SaladMaker_ ().prepare_salad (
        list_of_ingredients = SaladIngredient_values) (
        initial_bowl = Seq [SaladIngredient] () ) (
        next_ingredient_function = add_next_ingredient) (
        condition_to_continue = has_salad_at_most_2_ingredients
      )
    ) (
      expected = Seq (sunflower_seeds, lettuce, tomato)
    )
  )

}
