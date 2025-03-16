package soda.example.forlean.mathematics

/*
 * This package contains tests for examples of some mathematical functions that can be translated to Lean.
 */

case class FactorialForLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forlean.lib.IntNat
  import   soda.example.forlean.lib.Nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val factorial_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 1) , (1 , 1) , (2 , 2) , (3 , 6) , (4 , 24) , (5 , 120) , (6 , 720)
  )

  lazy val factorial_values_with_nat : Seq [ Tuple2 [Nat, Nat] ] =
    factorial_values
      .map ( pair =>
        Tuple2 (IntNat .mk .from_non_negative (pair ._1),
          IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the factorial function for Coq") (
    check (
      obtained = factorial_values_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , FactorialForLean .mk .get_factorial (n) ) )
    ) (
      expected = factorial_values_with_nat
    )
  )

}


case class FiboExampleInSodaForLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forlean.lib.IntNat
  import   soda.example.forlean.lib.Nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fibonacci_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 0) , (1 , 1) , (2 , 1) , (3 , 2) , (4 , 3) , (5 , 5) , (6 , 8) , (7 , 13) , (8 , 21) ,
    (9 , 34) , (10 , 55)
  )

  lazy val fibonacci_values_with_nat : Seq [Tuple2 [Nat, Nat] ] =
    fibonacci_values
      .map ( pair =>
        Tuple2 (IntNat .mk .from_non_negative (pair ._1),
          IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the fibonacci function for Lean") (
    check (
      obtained = fibonacci_values_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , FiboExampleInSodaForLean .mk .fib (n) ) )
    ) (
      expected = fibonacci_values_with_nat
    )
  )

}


case class TriangularNumberForLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forlean.lib.IntNat
  import   soda.example.forlean.lib.Nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val triangular_number_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 0) , (1 , 1) , (2 , 3) , (3 , 6) , (4 , 10) , (5 , 15) , (6 , 21)
  )

  lazy val triangular_number_with_nat : Seq [ Tuple2 [Nat, Nat] ] =
    triangular_number_values
      .map ( pair =>
        Tuple2 (IntNat .mk .from_non_negative (pair ._1) ,
          IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the triangular for Lean") (
    check (
      obtained = triangular_number_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , TriangularNumberForLean .mk .get_number (n) ) )
    ) (
      expected = triangular_number_with_nat
    )
  )

}

