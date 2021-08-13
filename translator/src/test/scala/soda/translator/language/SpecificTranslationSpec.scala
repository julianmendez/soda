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

  test ("should translate classes") {
    lazy val original =
      "* A =" +
      "\n" +
      "\n  f(x: Int): Int = x + 1" +
      "\n" +
      "\nclass B = {" +
      "\n" +
      "\n  g(x: Int): Int = 2 * x" +
      "\n}" +
      "\n" +
      "\nclass C() extends A =" +
      "\n" +
      "\n  h(x: Int): Int = 2 * x + 1" +
      "\n" +
      "\n* B[T] = {" +
      "\n" +
      "\n  i(x: T): T = x" +
      "\n}" +
      "\n"
    lazy val expected =
      "trait A:" +
      "\n" +
      "\n  def f (x: Int ): Int = x + 1" +
      "\n" +
      "\ntrait B {" +
      "\n" +
      "\n  def g (x: Int ): Int = 2 * x" +
      "\n}" +
      "\n" +
      "\ncase class C () extends A:" +
      "\n" +
      "\n  def h (x: Int ): Int = 2 * x + 1" +
      "\n" +
      "\ntrait B [T] {" +
      "\n" +
      "\n  def i (x: T ): T = x" +
      "\n}" +
      "\n"
    lazy val obtained = MicroTranslator_ () .translate_program (original )

    assert (obtained == expected )
  }
}

case class SpecificTranslationSpec_ () extends SpecificTranslationSpec
