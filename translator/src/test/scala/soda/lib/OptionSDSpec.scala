package soda.lib

import org.scalatest.funsuite.AnyFunSuite

case class OptionSDSpec () extends AnyFunSuite {

  test ("should test an empty option") {
    lazy val empty = NoneSD ()
    assert (empty.isEmpty )
  }

  test ("should test a non empty option") {
    lazy val element = SomeSD (1 )
    assert (! element.isEmpty )
  }

  test ("should open an empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: SomeSD [String]  ): String = "Its value is " + value.get + "."

    lazy val empty = NoneSD [String]  ()

    lazy val expected = "It is empty."
    lazy val obtained = empty.open (
      ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should open an non empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: SomeSD [String]  ): String = "Its value is " + value.get + "."

    lazy val some_element = SomeSD [String]  ("0")

    lazy val expected = "Its value is 0."
    lazy val obtained = some_element.open (
      ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should map empty to empty") {
    def to_string (n: Int ): String = "" + n

    lazy val empty = NoneSD [Int]  ()

    lazy val expected = NoneSD [String]  ()
    lazy val obtained = empty.map (to_string )

    assert (obtained == expected )
  }

  test ("should map a non-empty to another non-empty") {
    def to_string (n: Int ): String = "" + n

    lazy val some_element = SomeSD [Int]  (2 )

    lazy val expected = SomeSD [String]  ("2")
    lazy val obtained = some_element.map (to_string )

    assert (obtained == expected )
  }

}
