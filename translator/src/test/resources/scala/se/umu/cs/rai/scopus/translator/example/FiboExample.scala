package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec


trait Fibo_example {
  def fib(n: Int): Int
}


case class Fibo_example_in_scopus() extends Fibo_example {

  @tailrec
  def fa(m: Number, a: Number, b: Number) =
    if (m == 0) a
    else if (m == 1) b
    else fa(m - 1)(b)(a + b)


  def fib(n: Int) = fa(n, 0, 1)

}
