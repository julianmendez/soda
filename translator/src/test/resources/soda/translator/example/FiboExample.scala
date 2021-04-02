package soda.translator.example


trait FiboExample {

  def fib ( n: Int ) : Int

}


case class FiboExampleInSoda (  ) extends FiboExample {

  def fib ( n: Int ) = {
    lazy val result = rec ( n , 0 , 1 )

    def rec ( m: Int , a: Int , b: Int ) : Int =
      if ( m == 0 ) a
      else if ( m == 1 ) b
      else rec ( m - 1 , b , a + b )

    result
  }

}
