trait PairExample
{

  def   left : Int
  def   right : Int

}

case class PairExample_ (left : Int, right : Int) extends PairExample

object PairExample { def mk  (left : Int) (right : Int) : PairExample  = PairExample_  (left, right) }

trait SwapExample
{



  def swap (pair : PairExample) : PairExample =
    pair match  {
      case PairExample_ (a, b) =>
        PairExample_ (b, a)
    }

/*
  directive lean
  theorem
    swap_of_swap (pair : PairExample)
      : (swap (swap (pair) ) ) = pair := by
    rewrite [swap, swap]
    simp
*/

}

case class SwapExample_ () extends SwapExample

object SwapExample { def mk   : SwapExample  = SwapExample_  () }
