package soda.example.algorithms

case class LetBindingSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should evaluate an expression like 'where'")
    {
      lazy val obtained = LetBindingExample_ () .three_parts_like_where
      lazy val expected = Seq ("first part", "second part", "third part")
      assert (obtained == expected ) }

  test ("should evaluate an expression like 'let in'")
    {
      lazy val obtained = LetBindingExample_ () .three_parts_like_let_in
      lazy val expected = Seq ("first part", "second part", "third part")
      assert (obtained == expected ) }

  test ("should evaluate an expression like Coq 'let in'")
    {
      lazy val obtained = LetBindingExample_ () .three_parts_like_coq_let_in
      lazy val expected = Seq ("first part", "second part", "third part")
      assert (obtained == expected ) }

}
