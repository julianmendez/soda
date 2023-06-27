trait PairExample
{

  def   left : nat
  def   right : nat

}

case class PairExample_ (left : nat, right : nat) extends PairExample

trait SwapExample
{

  def left (pair : PairExample) : nat =
    pair match  {
      case (PairExample_ (x , y) ) => x
    }

  def right (pair : PairExample) : nat =
    pair match  {
      case (PairExample_ (x , y) ) => y
    }

  def swap (pair : PairExample) : PairExample =
    PairExample_ (right (pair) , left (pair) )

/*
  directive coq
  Theorem
    swap_of_swap : forall (pair : PairExample) , (swap (swap (pair) ) ) = pair .
  Proof.
    intros p.
    destruct p.
    compute.
    destruct x.
    apply eq_refl.
  Qed.
*/

}

case class SwapExample_ () extends SwapExample
