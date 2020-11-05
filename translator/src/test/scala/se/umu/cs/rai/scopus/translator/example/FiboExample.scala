package se.umu.cs.rai.scopus.translator.example

import scala.annotation.tailrec


trait Fibo_example {

  def fib (n: Int): Int

}


case class  Fibo_example_in_scopus () extends Fibo_example {

  def fa (m: Int, a: Int, b: Int): Int =
    if ( m == 0 ) a
    else if ( m == 1 ) b
    else fa (m - 1, b, a + b)

  def fib (n: Int) = fa (n, 0, 1)

}
