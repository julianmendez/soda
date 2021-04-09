package soda.translator.language

import org.scalatest.funsuite.AnyFunSuite


case class BeautifierSpec () extends AnyFunSuite {

  test ("the translated source code should not have too many unnecessary spaces") {
    lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
      "    original .  replaceAll(\"  \" ,  \" \")   \n"

    lazy val expected = "  def beautify_this  (original: String   ): String =\n" +
      "    original .replaceAll (\"  \", \" \")   \n"

    lazy val obtained = MicroTranslator () .translate_program (original )
    assert (obtained == expected )
  }

}
