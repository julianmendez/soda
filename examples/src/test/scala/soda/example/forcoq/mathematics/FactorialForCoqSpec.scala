package soda.example.forcoq.mathematics

case class FactorialForCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.example.forcoq.lib.nat
  import soda.example.forcoq.lib.IntNat_

  lazy val factorial_values: Seq [(Int, Int )] = Seq ((0, 1 ), (1, 1 ), (2, 2 ), (3, 6 ), (4, 24 ), (5, 120 ), (6, 720 )  )

  lazy val factorial_values_with_nat: Seq [(nat, nat )] =
    factorial_values
      .map (pair =>
        (IntNat_ () .from_non_negative (pair._1 ), IntNat_ () .from_non_negative (pair._2 ) )      )

  test ("should test the factorial function for Coq")
    {
      lazy val expected = factorial_values_with_nat
      lazy val obtained = factorial_values_with_nat
        .map (pair => pair._1 )
        .map (n => (n, FactorialForCoq_ () .get_factorial (n )  )  )
      assert (obtained == expected ) }

}
