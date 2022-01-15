package soda.translator.extension.toscala

trait ExampleWithWrongOrder {

  lazy val this_is_null_but = constant_defined_later

  lazy val constant_defined_later = "Success!"

}

case class ExampleWithWrongOrder_ () extends ExampleWithWrongOrder

trait ExampleWithRightOrder {

  lazy val constant_defined_before = "Success!"

  lazy val this_is_not_null = constant_defined_before

}

case class ExampleWithRightOrder_ () extends ExampleWithRightOrder

trait ExampleWithEmptyParentheses {

  def this_is_not_null  () = constant_defined_later

  lazy val constant_defined_later = "Success!"

}

case class ExampleWithEmptyParentheses_ () extends ExampleWithEmptyParentheses

trait AnotherExampleWithEmptyParentheses {

  lazy val this_is_not_null = constant_function_defined_later  ()

  def constant_function_defined_later  () = "Success!"

}

case class AnotherExampleWithEmptyParentheses_ () extends AnotherExampleWithEmptyParentheses

/**
 * In Soda constants cannot be defined as 'lazy val'.
 * These tests detect and test this problem, and test work-arounds.
 */
case class LazySyntaxSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should show what happens when constants are defined in the wrong order")
    {
      lazy val obtained = ExampleWithWrongOrder_ () .this_is_null_but
      lazy val expected = "Success!"
      assert (obtained == expected ) }

  test ("should show what happens when constants are defined in the right order")
    {
      lazy val obtained = ExampleWithRightOrder_ () .this_is_not_null
      lazy val expected = "Success!"
      assert (obtained == expected ) }

  test ("should show what happens when one work-around is used")
    {
      lazy val obtained = ExampleWithEmptyParentheses_ () .this_is_not_null  ()
      lazy val expected = "Success!"
      assert (obtained == expected ) }

  test ("should show what happens when another work-around is used")
    {
      lazy val obtained = AnotherExampleWithEmptyParentheses_ () .this_is_not_null
      lazy val expected = "Success!"
      assert (obtained == expected ) }

}
