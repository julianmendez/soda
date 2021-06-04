package soda.translator.example

case class Tuple2 (left: Int, right: Int )

case class SwapExample () {

  def swap (pair: Tuple2 ) =
    Tuple2 (pair.right, pair.left )
}
