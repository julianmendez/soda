package scopus.translator

import org.scalatest.funsuite.AnyFunSuite


case class ExampleWithWrongOrder() {

  val this_is_null = constant_defined_later

  val constant_defined_later = "Success!"

}

case class ExampleWithRightOrder() {

  val constant_defined_before = "Success!"

  val this_is_not_null = constant_defined_before

}

case class ExampleWithOneWorkAround() {

  def this_is_not_null () = constant_defined_later

  val constant_defined_later = "Success!"

}

case class ExampleWithAnotherWorkAround() {

  val this_is_not_null = constant_function_defined_later ()

  def constant_function_defined_later () = "Success!"

}


/**
 * In Scopus constants cannot be defined as 'lazy val'.
 * These tests detect and test this problem, and test work-arounds.
 */
case class LazySyntaxSpec() extends AnyFunSuite {

  test("should show what happens when constants are defined in the wrong order") {
    val obtained = ExampleWithWrongOrder().this_is_null
    assert(Option(obtained).isEmpty)
  }

  test("should show what happens when constants are defined in the right order") {
    val obtained = ExampleWithRightOrder().this_is_not_null
    val expected = "Success!"
    assert(obtained == expected)
  }

  test("should show what happens when one work-around is used") {
    val obtained = ExampleWithOneWorkAround().this_is_not_null ()
    val expected = "Success!"
    assert(obtained == expected)
  }

  test("should show what happens when another work-around is used") {
    val obtained = ExampleWithAnotherWorkAround().this_is_not_null
    val expected = "Success!"
    assert(obtained == expected)
  }

}
