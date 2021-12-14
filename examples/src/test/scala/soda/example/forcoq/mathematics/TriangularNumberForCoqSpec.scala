package soda.example.forcoq.mathematics

case class TriangularNumberForCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.example.forcoq.lib.nat
  import soda.example.forcoq.lib.IntNat_

  lazy val triangular_number_values: Seq [(Int, Int )] = Seq ((0, 0 ), (1, 1 ), (2, 3 ), (3, 6 ), (4, 10 ), (5, 15 ), (6, 21 )  )

  lazy val triangular_number_with_nat: Seq [(nat, nat )] =
    triangular_number_values
      .map (pair =>
        (IntNat_ () .from_non_negative  (pair._1 ), IntNat_ () .from_non_negative  (pair._2 ) )      )

  test ("should test the triangular for Coq")
    {
      lazy val expected = triangular_number_with_nat
      lazy val obtained = triangular_number_with_nat
        .map (pair => pair._1 )
        .map (n =>  (n, TriangularNumberForCoq_ () .get_number  (n )  )  )
      assert (obtained == expected ) }

}
