package soda.translator.example

case class PairExample (left: Int, right: Int )

case class SwapExample () {

  def swap (pair: PairExample ): PairExample =
    PairExample (pair.right, pair.left )
}
