package soda.example.forcoq.mathematics

case class FiboExampleInSodaForCoqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forcoq.lib.IntNat_
  import   soda.example.forcoq.lib.nat

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fibonacci_values: Seq [ Tuple2 [Int, Int] ] = Seq (
    (0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13), (8, 21), (9, 34), (10, 55)
  )

  lazy val fibonacci_values_with_nat: Seq [ Tuple2 [nat, nat] ] =
    fibonacci_values
      .map (  pair =>
        Tuple2 ( IntNat_ ().from_non_negative (pair._1), IntNat_ ().from_non_negative (pair._2) )
      )

  test ("should test the fibonacci function for Coq") (
    check (
      obtained = fibonacci_values_with_nat
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, FiboExampleInSodaForCoq_ ().fib (n) ) )
    ) (
      expected = fibonacci_values_with_nat
    )
  )

}
