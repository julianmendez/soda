package soda.example.algorithms

trait PairExample
{

  def   left : Int
  def   right : Int

}

case class PairExample_ (left : Int, right : Int) extends PairExample

trait SwapExample
{

  def left (pair : PairExample) : Int =
    pair match  {
      case (PairExample_ (x , y) ) => x
    }

  def right (pair : PairExample) : Int =
    pair match  {
      case (PairExample_ (x , y) ) => y
    }

  def swap (pair : PairExample) : PairExample =
    PairExample_ (right (pair) , left (pair) )

/*  theorem
    swap_of_swap : forall (pair : PairExample) , (swap (swap (pair) ) ) = pair
*/

/*  proof
    intros p.
    destruct p.
    compute.
    destruct x.
    apply eq_refl.
*/

}

case class SwapExample_ () extends SwapExample
