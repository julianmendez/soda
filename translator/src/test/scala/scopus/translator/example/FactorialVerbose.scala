package scopus.translator.example


trait AbstractFactorialVerbose {
  def factorial (n : Int) : Int
}

case class FactorialVerbose () extends AbstractFactorialVerbose {

  def factorial (n : Int) = factorial_rec (n, 1)

  import scala.annotation.tailrec
        @tailrec
  private
  def factorial_rec (n : Int, product : Int) : Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}
