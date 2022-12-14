package soda.example.forcoq.mathematics

case class TriangularNumberForCoqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forcoq.lib.IntNat_
  import   soda.example.forcoq.lib.nat

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val triangular_number_values : Seq [ Tuple2 [Int, Int] ] = Seq (
    (0, 0), (1, 1), (2, 3), (3, 6), (4, 10), (5, 15), (6, 21)
  )

  lazy val triangular_number_with_nat : Seq [ Tuple2 [nat, nat] ] =
    triangular_number_values
      .map (  pair =>
       Tuple2 ( IntNat_ ().from_non_negative (pair._1),  IntNat_ ().from_non_negative (pair._2) )
      )

  test ("should test the triangular for Coq") (
    check (
      obtained = triangular_number_with_nat
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, TriangularNumberForCoq_ ().get_number (n) ) )
    ) (
      expected = triangular_number_with_nat
    )
  )

}
