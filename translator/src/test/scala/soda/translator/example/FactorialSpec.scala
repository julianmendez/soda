package soda.translator.example


case class FactorialSpec () extends org.scalatest.funsuite.AnyFunSuite {

  lazy val factorial_values = Seq ((0, 1 ), (1, 1 ), (2, 2 ), (3, 6 ), (4, 24 ), (5, 120 ), (6, 720 ), (7, 5040 ), (8, 40320 ), (9, 362880 ), (10, 3628800 )  )

  test ("should test the factorial - concise version") {
    lazy val expected = factorial_values
    lazy val obtained = factorial_values
      .map (pair => pair._1 )
      .map (n => (n, FactorialConciseImpl () .factorial (n )  )  )

    assert (obtained == expected )
  }

  test ("should test the factorial - verbose version") {
    lazy val expected = factorial_values
    lazy val obtained = factorial_values
      .map (pair => pair._1 )
      .map (n => (n, FactorialVerboseImpl () .factorial (n )  )  )
    assert (obtained == expected )
  }
}
