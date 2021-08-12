package soda.translator.language


trait BeautifierSpec  extends org.scalatest.funsuite.AnyFunSuite {

  test ("the translated source code should not have too many unnecessary spaces") {
    lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
      "    original .  replaceAll(\"  \" ,  \" \")   \n"
    lazy val expected = "  def beautify_this  (original: String   ): String =\n" +
      "    original .replaceAll (\"  \", \" \")   \n"
    lazy val obtained = MicroTranslator_ () .translate_program (original )

    assert (obtained == expected )
  }
}

case class BeautifierSpec_ () extends BeautifierSpec
