package scopus.translator.example

import scala.annotation.tailrec

trait AbstractFactorialConcise {
  def factorial (n: Int): Int
}

case class FactorialConcise () extends AbstractFactorialConcise {

  def factorial (n: Int) = __factorial_rec (n, 1)

  import scala.annotation.tailrec
  @tailrec final
  def __factorial_rec (n: Int, product: Int): Int =
    if ( n == 0
    ) product
    else __factorial_rec (n - 1, n * product)

}
