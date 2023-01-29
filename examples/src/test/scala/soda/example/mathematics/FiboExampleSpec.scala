package soda.example.mathematics

case class FiboExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fibonacci_values = Seq (
   (0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13), (8, 21), (9, 34), (10, 55)
  )

  private lazy val _fibo_example_in_soda = FiboExampleInSoda_ ()

  test ("should test the fibonacci function") (
    check (
      obtained = fibonacci_values
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, _fibo_example_in_soda.apply (n) ) )
    ) (
      expected = fibonacci_values
    )
  )

}
