package soda.example.algorithms

trait PairExample {

  def left: Int

  def right: Int

}

case class PairExample_ (left: Int, right: Int )  extends PairExample

trait SwapExample {

  def swap (pair: PairExample ): PairExample =
    PairExample_ (pair.right, pair.left )

  /*
  theorem
    swap_of_swap: forall (pair: PairExample), (swap (swap (pair) ) ) == pair

  proof
    intros p.
    destruct p.
    compute.
    apply eq_refl.
  */

}
