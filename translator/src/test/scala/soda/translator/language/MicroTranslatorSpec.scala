package soda.translator.language


case class MicroTranslatorSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should join lines ending in comma") {
    lazy val input = Seq ("this ", "should join,", "all the lines , ", "  that end with", "comma.")
    lazy val expected = Seq ("this ", "should join,all the lines ,   that end with", "comma.")
    lazy val obtained = MicroTranslator () .join_lines_with_opening_brackets (input )

    assert (obtained == expected )
  }
}
