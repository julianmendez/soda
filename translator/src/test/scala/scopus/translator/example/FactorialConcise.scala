package scopus.translator.example


trait AbstractFactorialConcise {
  def factorial (n: Int): Int
}

case class FactorialConcise () extends AbstractFactorialConcise {

  def factorial (n: Int) = factorial_rec (n, 1)

  import scala.annotation.tailrec
        @tailrec
  private
  def factorial_rec (n: Int, product: Int): Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}
