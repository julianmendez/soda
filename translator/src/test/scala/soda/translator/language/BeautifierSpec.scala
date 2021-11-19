package soda.translator.language

case class BeautifierSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("the translated source code should not have too many unnecessary spaces")
    {
      lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
        "    original .  replaceAll(\"  \" ,  \" \")   \n"
      lazy val expected = "  def beautify_this  (original: String   ): String =\n" +
        "    original .replaceAll (\"  \", \" \")   \n"
      lazy val obtained = MicroTranslatorToScala_ () .translate_program (original )
      assert (obtained == expected ) }

}
