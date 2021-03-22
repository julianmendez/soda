package scopus.translator.example


trait FiboExample {

  def fib (n : Int) : Int

}


case class FiboExampleInScopus () extends FiboExample {

  def fib (n : Int) = {
    lazy val result = fa (n, 0, 1)

    def fa (m : Int, a : Int, b : Int) : Int =
      if ( m == 0 ) a
      else if ( m == 1 ) b
      else fa (m - 1, b, a + b)

    result
  }

}
