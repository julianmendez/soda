package scopus.translator.example


trait Abstract_factorial_verbose {
  def factorial (n : Int) : Int
}

case class Factorial_verbose () extends Abstract_factorial_verbose {

  def factorial (n : Int) = factorial_rec (n, 1)

  import scala.annotation.tailrec
        @tailrec
  private
  def factorial_rec (n : Int, product : Int) : Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}
