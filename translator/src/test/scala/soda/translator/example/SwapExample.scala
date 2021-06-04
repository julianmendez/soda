package soda.translator.example

case class PairExample (left: Int, right: Int )

case class SwapExample () {

  def swap (pair: PairExample ) =
    Tuple2 (pair.right, pair.left )
}
