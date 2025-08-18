package soda.example.forrocq.mathematics

/*
 * This package contains tests for examples of some mathematical functions that can be translated to Rocq.
 */

case class FactorialForRocqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forrocq.lib.IntNat
  import   soda.example.forrocq.lib.nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val factorial_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 1) , (1 , 1) , (2 , 2) , (3 , 6) , (4 , 24) , (5 , 120) , (6 , 720)
  )

  lazy val factorial_values_with_nat : Seq [ Tuple2 [nat, nat] ] =
    factorial_values
      .map ( pair =>
        Tuple2 (IntNat .mk .from_non_negative (pair ._1),
          IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the factorial function for Rocq") (
    check (
      obtained = factorial_values_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , FactorialForRocq .mk .get_factorial (n) ) )
    ) (
      expected = factorial_values_with_nat
    )
  )

}


case class FiboExampleInSodaForRocqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forrocq.lib.IntNat
  import   soda.example.forrocq.lib.nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fibonacci_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 0) , (1 , 1) , (2 , 1) , (3 , 2) , (4 , 3) , (5 , 5) , (6 , 8) , (7 , 13) ,
      (8 , 21) , (9 , 34) , (10 , 55)
  )

  lazy val fibonacci_values_with_nat : Seq [Tuple2 [nat, nat] ] =
    fibonacci_values
      .map ( pair =>
        Tuple2 (IntNat .mk .from_non_negative (pair ._1) ,
          IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the fibonacci function for Rocq") (
    check (
      obtained = fibonacci_values_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , FiboExampleInSodaForRocq .mk .fib (n) ) )
    ) (
      expected = fibonacci_values_with_nat
    )
  )

}


case class TriangularNumberForRocqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.example.forrocq.lib.IntNat
  import   soda.example.forrocq.lib.nat

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val triangular_number_values : Seq [Tuple2 [Int, Int] ] = Seq (
    (0 , 0) , (1 , 1) , (2 , 3) , (3 , 6) , (4 , 10) , (5 , 15) , (6 , 21)
  )

  lazy val triangular_number_with_nat : Seq [ Tuple2 [nat, nat] ] =
    triangular_number_values
      .map ( pair =>
       Tuple2 (IntNat .mk .from_non_negative (pair ._1) ,
         IntNat .mk .from_non_negative (pair ._2) )
      )

  test ("should test the triangular for Rocq") (
    check (
      obtained = triangular_number_with_nat
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , TriangularNumberForRocq .mk .get_number (n) ) )
    ) (
      expected = triangular_number_with_nat
    )
  )

}

