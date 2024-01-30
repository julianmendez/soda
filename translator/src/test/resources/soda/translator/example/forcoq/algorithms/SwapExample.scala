trait PairExample
{

  def   left : nat
  def   right : nat

}

case class PairExample_ (left : nat, right : nat) extends PairExample

object PairExample {
  def mk (left : nat) (right : nat) : PairExample =
    PairExample_ (left, right)
}

trait SwapExample
{



  def swap (pair : PairExample) : PairExample =
    PairExample_ (pair .right , pair .left )

/*
  directive coq
  Theorem
    swap_of_swap : forall (x : nat) (y : nat) , (swap (swap (PairExample_ (x, y) ) ) ) =
    PairExample_ (x, y) .
  Proof.
    auto.
  Qed.
*/

}

case class SwapExample_ () extends SwapExample

object SwapExample {
  def mk : SwapExample =
    SwapExample_ ()
}
