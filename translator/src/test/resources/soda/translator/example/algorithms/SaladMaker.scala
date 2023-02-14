
trait SaladMaker
{

  def apply [Ingredient, Salad] (list_of_ingredients : Seq [Ingredient] ) (initial_bowl : Salad) (next_ingredient_function : Salad => Ingredient => Salad) (condition_to_continue : Salad => Ingredient => Boolean) : Salad =
    _tailrec_prepare_salad (list_of_ingredients) (initial_bowl) (next_ingredient_function) (condition_to_continue)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_prepare_salad [Ingredient, Salad] (ingredients_so_far : Seq [Ingredient] ) (salad_so_far : Salad) (next_ingredient_function : Salad => Ingredient => Salad) (condition_to_continue : Salad => Ingredient => Boolean) : Salad =
    if ( ingredients_so_far.isEmpty || ( ! condition_to_continue (salad_so_far) (ingredients_so_far.head) )
    ) salad_so_far
    else _tailrec_prepare_salad (ingredients_so_far.tail) (next_ingredient_function (salad_so_far) (ingredients_so_far.head) ) (next_ingredient_function) (condition_to_continue)

}

case class SaladMaker_ () extends SaladMaker
