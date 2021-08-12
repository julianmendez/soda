package soda.translator.language


trait SpecificTranslationSpec  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should translate a small snippet") {
    lazy val original = "  input_lines = Seq(" +
      "\n    \"  f( x: Int,\\t\", " +
      "\n    \"     y: Int) =\"," +
      "\n    \"       x + y\")" +
      "\n"
    lazy val expected = "  lazy val input_lines = Seq (" +
      "\"  f( x: Int,\\t\", " +
      "\"     y: Int) =\"," +
      " \"       x + y\")" +
      "\n"
    lazy val obtained = MicroTranslator_ () .translate_program (original )

    assert (obtained == expected )
  }

  test ("should leave content of apostrophes unchanged") {
    lazy val input = " a = Seq('\\'', \'', '\\\"', ' or ', \'or\', '0x00', '->', '/*', '*/')\n"
    lazy val expected = " lazy val a = Seq ('\\'', '', '\\\"', ' or ', 'or', '0x00', '->', '/*', '*/')\n"
    lazy val obtained = MicroTranslator_ () .translate_program (input )

    assert (obtained == expected )
  }

  test ("should leave content of quotation marks unchanged") {
    lazy val input = " a = Seq(\"\\\"\", \"\", \"\\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
    lazy val expected = " lazy val a = Seq (\"\\\"\", \"\", \"\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
    lazy val obtained = MicroTranslator_ () .translate_program (input )

    assert (obtained == expected )
  }
}

case class SpecificTranslationSpec_ () extends SpecificTranslationSpec
