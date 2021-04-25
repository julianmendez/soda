package soda.collection

import org.scalatest.funsuite.AnyFunSuite

case class OptionSDSpec () extends AnyFunSuite {

  test ("should create an OptionSD and test isEmpty") {
    lazy val empty = NoneSD ()
    assert (empty.isEmpty )

    lazy val element = SomeSD (1 )
    assert (! element.isEmpty )
  }

  test ("should create an OptionSD and test open") {
     lazy val result_if_empty: String = "It is empty."
     def result_if_non_empty (value: SomeSD [String]  ): String = "Its value is " + value.get + "."

     lazy val empty = NoneSD [String]  ()

     lazy val obtained_if_empty = empty.open (
       ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

     assert (obtained_if_empty == "It is empty.")

     lazy val some_element = SomeSD [String]  ("0")

     lazy val obtained_if_non_empty = some_element.open (
       ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

     assert (obtained_if_non_empty == "Its value is 0.")
  }

}
