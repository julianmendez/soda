package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec


trait  Fibo_example {

  def fib(n: Number): Number

}


case class Fibo_example_in_scopus() extends Fibo_example {

  def fa(m: Number, a: Number, b: Number) =
    if m == 0 then a
    else if m == 1 then b
    else fa(m - 1, b, a + b)

  def fib(n: Number) = fa(n, 0, 1)

}
