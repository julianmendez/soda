trait SaladMaker
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_prepare_salad [Ingredient , Salad ]
    (ingredients : Seq [Ingredient] )
    (salad : Salad)
    (next_ingredient : Salad => Ingredient => Salad)
    (condition : Salad => Ingredient => Boolean)
      : Salad =
    if ( ingredients .isEmpty ||
      (! condition (salad) (ingredients .head) )
    ) salad
    else _tailrec_prepare_salad (ingredients .tail) (
      next_ingredient (salad) (ingredients .head) ) (next_ingredient) (condition)

  def apply [Ingredient , Salad ]
    (list_of_ingredients : Seq [Ingredient] )
    (initial_bowl : Salad)
    (next_ingredient : Salad => Ingredient => Salad)
    (condition : Salad => Ingredient => Boolean)
      : Salad =
    _tailrec_prepare_salad (list_of_ingredients) (initial_bowl) (next_ingredient) (condition)

}

case class SaladMaker_ () extends SaladMaker
