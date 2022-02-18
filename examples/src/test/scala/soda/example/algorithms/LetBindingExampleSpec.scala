package soda.example.algorithms

case class LetBindingSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should evaluate an expression like 'where'") (
    check (
      obtained = LetBindingExample_ ().three_parts_like_where
    ) (
      expected = Seq ("first part", "second part", "third part")
    )
  )

  test ("should evaluate an expression like 'let in'") (
    check (
      obtained = LetBindingExample_ ().three_parts_like_let_in
    ) (
      expected = Seq ("first part", "second part", "third part")
    )
  )

  test ("should evaluate an expression like Coq 'let in'") (
    check (
      obtained = LetBindingExample_ ().three_parts_like_coq_let_in
    ) (
      expected = Seq ("first part", "second part", "third part")
    )
  )

}
