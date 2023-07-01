trait PairExample
{

  def   left : Int
  def   right : Int

}

case class PairExample_ (left : Int, right : Int) extends PairExample

trait SwapExample
{

  def swap (pair : PairExample) : PairExample =
    PairExample_ (pair .right, pair .left)

/*
  directive lean
  theorem
    swap_of_swap (x : Int) (y : Int) : (swap (swap (PairExample_ (x, y) ) ) ) = PairExample_ (x, y) :=
      by
        constructor
*/

}

case class SwapExample_ () extends SwapExample
