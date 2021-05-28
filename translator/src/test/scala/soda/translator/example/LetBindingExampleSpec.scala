package soda.translator.example


case class LetBindingSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should evaluate an expression like 'where'") {
    lazy val obtained = LetBindingExample () .three_parts_like_where
    lazy val expected = Seq ("first part", "second part", "third part")

    assert (obtained == expected )
  }

  test ("should evaluate an expression like 'let in'") {
    lazy val obtained = LetBindingExample () .three_parts_like_let_in
    lazy val expected = Seq ("first part", "second part", "third part")

    assert (obtained == expected )
  }
}
