package soda.example.forcoq.mathematics

case class FactorialForCoqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forcoq.lib.IntNat_
  import   soda.example.forcoq.lib.nat

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val factorial_values : Seq [ Tuple2 [Int, Int] ] = Seq (
    (0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720)
  )

  lazy val factorial_values_with_nat : Seq [ Tuple2 [nat, nat] ] =
    factorial_values
      .map (  pair =>
        Tuple2 ( IntNat_ ().from_non_negative (pair._1), IntNat_ ().from_non_negative (pair._2) )
      )

  test ("should test the factorial function for Coq") (
    check (
      obtained = factorial_values_with_nat
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, FactorialForCoq_ ().get_factorial (n) ) )
    ) (
      expected = factorial_values_with_nat
    )
  )

}
