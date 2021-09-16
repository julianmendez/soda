package soda.example

trait PairExample {

  def left: Int

  def right: Int
}

case class PairExample_ (left: Int, right: Int ) extends PairExample

trait SwapExample {

  def swap (pair: PairExample ): PairExample =
    PairExample_ (pair.right, pair.left )
}
