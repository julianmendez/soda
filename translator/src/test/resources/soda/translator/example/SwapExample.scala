package soda.translator.example

case class Pair (left:Int, right: Int )

case class SwapExample () {
    def swap (pair: Pair ) =
         Pair (pair.right, pair.left )
}
