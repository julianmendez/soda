package soda.example.mathematics

case class FiboExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val fibonacci_values = Seq (
   (0, 0 ), (1, 1 ), (2, 1 ), (3, 2 ), (4, 3 ), (5, 5 ), (6, 8 ), (7, 13 ), (8, 21 ), (9, 34 ), (10, 55 )
  )

  test ("should test the fibonacci function")
    {
      lazy val expected = fibonacci_values
      lazy val obtained = fibonacci_values
        .map (pair => pair._1 )
        .map (n => (n, FiboExampleInSoda_ () .fib (n )  )  )
      assert (obtained == expected ) }

}
