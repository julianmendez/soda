package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class SpecificTranslationSpec() extends AnyFunSuite {


  val original = "  input_lines = Seq(" +
    "\n    \"  f( x: Int,\\t\", " +
    "\n    \"     y: Int) =\"," +
    "\n    \"       x + y\")" +
    "\n"

  val expected = "  val input_lines = Seq(" +
    "\n    \"  f( x: Int,\\t\", " +
    "    \"     y: Int) =\"," +
    "    \"       x + y\")" +
    "\n"

  test("should translate a small snippet") {
    val obtained = MicroTranslator().translate_program(original)
    assert(obtained == expected)
  }

}
