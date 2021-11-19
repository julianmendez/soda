package soda.example.forcoq

case class FiboExampleInSodaForCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.example.forcoq.lib.nat
  import soda.example.forcoq.lib.IntNat_

  lazy val fibonacci_values: Seq [(Int, Int )] = Seq ((0, 0 ), (1, 1 ), (2, 1 ), (3, 2 ), (4, 3 ), (5, 5 ), (6, 8 ), (7, 13 ), (8, 21 ), (9, 34 ), (10, 55 )  )

  lazy val fibonacci_values_with_nat: Seq [(nat, nat )] =
    fibonacci_values
      .map (pair =>
        (IntNat_ () .from_non_negative (pair._1 ), IntNat_ () .from_non_negative (pair._2 ) )      )

  test ("should test the fibonacci function for Coq")
    {
      lazy val expected = fibonacci_values_with_nat
      lazy val obtained = fibonacci_values_with_nat
        .map (pair => pair._1 )
        .map (n => (n, FiboExampleInSodaForCoq_ () .fib (n )  )  )
      assert (obtained == expected ) }

}
