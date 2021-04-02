package scopus.translator.example


trait AbstractFactorialVerbose {
  def factorial ( n: Int ) : Int
}

case class FactorialVerbose (  ) extends AbstractFactorialVerbose {

  def factorial ( n: Int ) = {
    lazy val result = rec ( n , 1 )

    import scala.annotation.tailrec
        @tailrec
    def rec ( n: Int , product: Int ) : Int =
      if ( n == 0
      ) product
      else rec ( n - 1 , n * product )

    result
  }
}