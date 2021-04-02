package soda.translator.example


case class SwapExample (  ) {
    def swap ( pair: ( Int , Int )  ) =
         ( pair._2 , pair._1 )
}
