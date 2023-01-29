package soda.example.mathematics

case class FactorialSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val factorial_values = Seq (
    (0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720), (7, 5040), (8, 40320), (9, 362880), (10, 3628800)
  )

  private lazy val _factorial_concise = FactorialConcise_ ()

  private lazy val _factorial_patten_matching = FactorialPatternMatching_ ()

  private lazy val _factorial_with_fold = FactorialWithFold_ ()

  test ("should test the factorial - concise version") (
    check (
      obtained = factorial_values
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, _factorial_concise.apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

  test ("should test the factorial - with pattern matching") (
    check (
      obtained = factorial_values
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, _factorial_patten_matching.apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

  test ("should test the factorial - with fold") (
    check (
      obtained = factorial_values
        .map (  pair => pair._1)
        .map (  n => Tuple2 (n, _factorial_with_fold.apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

}
