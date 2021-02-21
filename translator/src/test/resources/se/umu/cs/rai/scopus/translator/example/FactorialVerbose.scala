package se.umu.cs.rai.scopus.translator.example

import scala.annotation.tailrec

trait AbstractFactorialVerbose {
  def factorial (n : Int) : Int
}

case class FactorialVerbose () extends AbstractFactorialVerbose {

  def factorial (n : Int) = __factorial_rec (n, 1)

  @tailrec final
  def __factorial_rec (n : Int, product : Int) : Int =
    if ( n == 0
    ) product
    else __factorial_rec (n - 1, n * product)

}
