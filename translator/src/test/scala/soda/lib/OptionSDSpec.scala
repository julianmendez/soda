package soda.lib

import scala.util.Try
import scala.util.Success
import scala.util.Failure
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
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."

    lazy val empty = NoneSD [String]  ()

    lazy val expected = "It is empty."
    lazy val obtained = empty.open (
      ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should open an non empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."

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

  test ("should try how successive applications of open works") {

    def toInt (s: String ): OptionSD [Int] =
      OptionSDBuilder () .build (
        Try (Integer.parseInt (s.trim ) )
          .toOption
      )

    lazy val stringA = "1"
    lazy val stringB = "2"
    lazy val stringC = "3"

    lazy val maybeA = toInt (stringA )
    lazy val maybeB = toInt (stringB )
    lazy val maybeC = toInt (stringC )

    lazy val expected = SomeSD (6 )
    lazy val obtained =
      maybeA.open (ifEmpty = NoneSD, ifNonEmpty = a =>
          maybeB.open (ifEmpty = NoneSD, ifNonEmpty = b =>
              maybeC.open (ifEmpty = NoneSD, ifNonEmpty = c =>
                  SomeSD (a + b + c ) ) ) )

    assert (obtained == expected )
  }

  test ("toOption with non empty option") {
    lazy val input: OptionSD [Int] = SomeSD (1 )
    lazy val expected: Option [Int] = Some (1 )
    lazy val obtained = input.toOption
    assert (obtained == expected )
  }

  test ("toOption with another non empty option") {
    lazy val input: SomeSD [Int] = SomeSD (2 )
    lazy val expected: Some [Int] = Some (2 )
    lazy val obtained = input.toOption
    assert (obtained == expected )
  }

  test ("toOption with empty option") {
    lazy val input: OptionSD [Int] = NoneSD ()
    lazy val expected = None
    lazy val obtained = input.toOption
    assert (obtained == expected )
  }

}