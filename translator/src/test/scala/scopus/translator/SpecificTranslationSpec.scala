package scopus.translator

import org.scalatest.funsuite.AnyFunSuite


case class SpecificTranslationSpec() extends AnyFunSuite {

  lazy val original = "  input_lines = Seq(" +
    "\n    \"  f( x: Int,\\t\", " +
    "\n    \"     y: Int) =\"," +
    "\n    \"       x + y\")" +
    "\n"

  lazy val expected = "  lazy val input_lines = Seq(" +
    "\n    \"  f( x: Int,\\t\", " +
    "    \"     y: Int) =\"," +
    "    \"       x + y\")" +
    "\n"

  test("should translate a small snippet") {
    lazy val obtained = MicroTranslator().translate_program(original)
    assert(obtained == expected)
  }

}
